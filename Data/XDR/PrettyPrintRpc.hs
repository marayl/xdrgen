-- | FIXME: Mark describe sun rpc/openxdr.

module Data.XDR.PrettyPrintRpc
    ( ppRpcHeader
    , ppRpcImpl
    ) where

import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.XDR.AST
import Data.XDR.PPKeywords
import Data.XDR.PPUtils
import System.Path hiding ((</>))
import Text.PrettyPrint.Leijen as PP hiding (braces, indent)

indent :: Int
indent = 4

braces :: Doc -> Doc
braces d = nest indent (lbrace <$> d) <$> rbrace

switchBraces :: Doc -> Doc
switchBraces = enclose (lbrace <> line) (line <> rbrace)

moduleToString :: ModuleName -> String -> String
moduleToString (ModuleName xs) sep =
    concat . intersperse sep $ xs

fileGuard :: ModuleName -> Doc
fileGuard mod = text ("XDR_" ++ map toUpper (moduleToString mod "_") ++ "_H")

-- XDR
ppDefaultConstant :: Doc -> Maybe ConstExpr -> Doc
ppDefaultConstant d =
    maybe d ppConstExpr

ppSizeOf :: TypeSpec -> Doc
ppSizeOf t =
    kSizeof <> parens (ppTypeSpec t)

ppIfDecode :: Doc
ppIfDecode =
    kIf <+> parens (text "XDR_DECODE == xdrs->x_op")

ppIfFalse :: Doc -> Doc
ppIfFalse d =
    kIf <+> parens (char '!' <> d </> text "&& XDR_FREE != xdrs->x_op")

ppReturn :: String -> Doc
ppReturn =
    (<> semi) . (kReturn <+>) . text

ppGoto :: (Show a) => a -> Doc
ppGoto n =
    kGoto <+> text ("xfree" ++ show n) <> semi

ppIfFalseReturn :: Doc -> Doc
ppIfFalseReturn d =
    nest indent (ppIfFalse d <$> ppReturn "FALSE")

ppIfFalseGoto :: (Show a) => a -> Doc -> Doc
ppIfFalseGoto n d =
    nest indent (ppIfFalse d <$> ppGoto n)

ppUnwind :: (Show a) => a -> Doc -> Doc
ppUnwind n d =
    text ("xfree" ++ show n) <> colon
             <$> nest indent (ppIfDecode <$> d) <> semi

ppXFreeInit :: Doc
ppXFreeInit =
    text "XDR" <+> text "xfree" <> semi
             <$> text "xfree.x_op" <+> equals <+> text "XDR_FREE" <> semi

ppFuncSig :: TypeDecl -> Doc
ppFuncSig (SimpleDecl n s) =
    text "bool_t" <$> text ("xdr_" ++ n) <> ppFuncParam n s
ppFuncSig d =
    text "bool_t" <$> text ("xdr_" ++ n)
             <> tupled [text "XDR *xdrs", text (n ++ " *objp")]
  where
    n = declName d

ppFuncParam :: String -> SimpleSpec -> Doc
ppFuncParam n (ArraySpec _ _) =
    tupled [text "XDR *xdrs", text (n ++ " objp")]
ppFuncParam n (OpaqueSpec _) =
    tupled [text "XDR *xdrs", text (n ++ " objp")]
ppFuncParam n _ =
    tupled [text "XDR *xdrs", text (n ++ " *objp")]

ppCallEnum :: String -> String -> Doc
ppCallEnum xdrs ptr =
    text "xdr_enum"
             <> tupled [text xdrs,
                        text ("(enum_t *)" ++ ptr)]

ppCallTypeSpec :: String -> String -> TypeSpec -> Doc
ppCallTypeSpec xdrs ptr BoolSpec =
    text "xdr_bool"
             <> tupled [text xdrs,
                        text ptr]
ppCallTypeSpec xdrs ptr HyperSpec =
    text "xdr_hyper"
             <> tupled [text xdrs,
                        text ptr]
ppCallTypeSpec xdrs ptr UHyperSpec =
    text "xdr_u_hyper"
             <> tupled [text xdrs,
                        text ptr]

ppCallTypeSpec xdrs ptr t =
    text "xdr_" <> ppTypeSpec t
             <> tupled [text xdrs,
                        text ptr]

ppCallVector :: String -> String -> ConstExpr -> TypeSpec -> Doc
ppCallVector xdrs ptr c t =
    text "xdr_vector"
             <> tupled [text xdrs,
                        text ("(char *)" ++ ptr),
                        ppConstExpr c,
                        ppSizeOf t,
                        text "(xdrproc_t)xdr_" <> ppTypeSpec t]

ppCallArray :: String -> String -> String -> Maybe ConstExpr -> TypeSpec
            -> Doc
ppCallArray xdrs ptr len mc t =
    text "xdr_array"
             <> tupled [text xdrs,
                        text ("(char **)" ++ ptr),
                        text ("(u_int *)" ++ len),
                        ppDefaultConstant (text "~0") mc,
                        ppSizeOf t,
                        text "(xdrproc_t)xdr_" <> ppTypeSpec t]

ppCallOpaque :: String -> String -> ConstExpr -> Doc
ppCallOpaque xdrs ptr c =
    text "xdr_opaque"
             <> tupled [text xdrs,
                        text ptr,
                        ppConstExpr c]

ppCallBytes :: String -> String -> String -> Maybe ConstExpr -> Doc
ppCallBytes xdrs ptr len mc =
    text "xdr_bytes"
             <> tupled [text xdrs,
                        text ("(char **)" ++ ptr),
                        text ("(u_int *)" ++ len),
                        ppDefaultConstant (text "~0") mc]

ppCallString :: String -> String -> Maybe ConstExpr -> Doc
ppCallString xdrs ptr mc =
    text "xdr_string"
             <> tupled [text xdrs,
                        text ptr,
                        ppDefaultConstant (text "~0") mc]

ppCallPointer :: String -> String -> TypeSpec -> Doc
ppCallPointer xdrs ptr t =
    text "xdr_pointer"
             <> tupled [text xdrs,
                        text ("(char **)" ++ ptr),
                        ppSizeOf t,
                        text "(xdrproc_t)xdr_" <> ppTypeSpec t]

ppTypeSpec :: TypeSpec -> Doc
ppTypeSpec IntSpec = kInt
ppTypeSpec UIntSpec = text "u_int"
ppTypeSpec HyperSpec = text "quad_t"
ppTypeSpec UHyperSpec = text "u_quad_t"
ppTypeSpec FloatSpec = kFloat
ppTypeSpec DoubleSpec = kDouble
ppTypeSpec QuadrupleSpec = error "quadruple not supported"
ppTypeSpec BoolSpec = text "bool_t"
ppTypeSpec (NamedSpec n) = text n

ppIncludes :: [ModuleName] -> Doc
ppIncludes =
    vcat . map ppInclude

ppInclude :: ModuleName -> Doc
ppInclude mod =
    text "#include" <+> text f
  where
    f = "\"" ++ moduleToString mod "/" ++ ".h\""

-- | Pretty print a C header for use with the sun rpc library.
ppRpcHeader :: ModuleSpec -> String
ppRpcHeader spec =
    show $ header <$> ppSpec spec <$> ppFuncs spec <$> footer
  where
    header = vcat [text "#ifndef" <+> compileGuard,
                   text "#define" <+> compileGuard,
                   text "#include <rpc/xdr.h>",
                   ppIncludes . M.keys $ moduleImports spec]
    footer = text "#endif /*" <+> compileGuard <+> text "*/"
    compileGuard = fileGuard $ moduleName spec

    ppSpec :: ModuleSpec -> Doc
    ppSpec (ModuleSpec _ _ ds) =
        f ds
      where
        f = vcat . punctuate linebreak . map ppDef

    ppDef :: Definition -> Doc
    ppDef (TypeDef td) =
        kTypedef <+> ppTypeDecl td <> semi
    ppDef (ConstDef cd) =
        ppConstDecl cd

    ppTypeDecl :: TypeDecl -> Doc
    ppTypeDecl (EnumDecl n s) =
        kEnum <+> ppEnumSpec s <+> text n
    ppTypeDecl (StructDecl n s) =
        kStruct <+> ppStructSpec s <+> text n
    ppTypeDecl (UnionDecl n s) =
        kStruct <+> ppUnionSpec s <+> text n
    ppTypeDecl (SimpleDecl n s) =
        ppSimpleSpec n s

    ppConstDecl :: ConstDecl -> Doc
    ppConstDecl (ConstDecl n c) =
        text "#define" <+> text n <+> ppConstExpr c

    ppEnumSpec :: EnumSpec -> Doc
    ppEnumSpec (EnumSpec cs) =
        braces . vcat . punctuate comma . map ppEnumConstDecl $ cs

    ppEnumConstDecl :: ConstDecl -> Doc
    ppEnumConstDecl (ConstDecl n c) =
        text n <+> text "=" <+> ppConstExpr c

    ppStructSpec :: StructSpec -> Doc
    ppStructSpec (StructSpec ds) =
        braces . vcat . punctuate semi . map ppTypeDecl $ ds

    ppUnionSpec :: UnionSpec -> Doc
    ppUnionSpec (UnionSpec (UnionDis n t) cs md) =
        braces body
      where
        body = ppTypeSpec t <+> text n <> semi
               <$> kUnion <+> (braces . vcat $ (catMaybes xs ++ ys))
                       <+> text "u" <> semi
        xs = map (ppUnionArm . snd) cs
        ys = maybeToList . ppUnionDflt $ md

    ppUnionDflt :: Maybe UnionArm -> Maybe Doc
    ppUnionDflt ma =
        ma >>= ppUnionArm

    ppUnionArm :: UnionArm -> Maybe Doc
    ppUnionArm (DeclArm d) =
        Just . (<> semi) . ppTypeDecl $ d
    ppUnionArm VoidArm =
        Nothing

    ppSimpleSpec :: String -> SimpleSpec -> Doc
    ppSimpleSpec n (PlainSpec t) =
        ppTypeSpec t <+> text n
    ppSimpleSpec n (ArraySpec t c) =
        ppTypeSpec t <+> text n <> (brackets . ppConstExpr $ c)
    ppSimpleSpec n (VarArraySpec t _) =
        ppVarStruct n (ppTypeSpec t)
    ppSimpleSpec n (OpaqueSpec c) =
        kChar <+> text n <> (brackets . ppConstExpr $ c)
    ppSimpleSpec n (VarOpaqueSpec _) =
        ppVarStruct n (text "char")
    ppSimpleSpec n (StringSpec _) =
        kChar <+> char '*' <> text n
    ppSimpleSpec n (PointerSpec t) =
        ppTypeSpec t <+> char '*' <+> text n

    ppVarStruct :: String -> Doc -> Doc
    ppVarStruct n td =
        kStruct <+> braces (text "u_int" <+> text "len" <> semi
                            <$> td <+> text "*val" <> semi)
                    <+> text n

    ppFuncs (ModuleSpec _ _ ds) =
        vcat . map f $ typeDecls ds
      where
        f = (<> semi) . ppFuncSig

-- | Pretty print a C implementation for use with the sun rpc library.
ppRpcImpl :: ModuleSpec -> String
ppRpcImpl spec = show $ ppInclude (moduleName spec) <$> ppSpec spec
  where
    ppSpec (ModuleSpec _ _ ds) =
        f ds
      where
        f = vcat . punctuate linebreak . map ppTypeDecl . typeDecls

    ppTypeDecl :: TypeDecl -> Doc
    ppTypeDecl d = ppFuncSig d <$> ppFuncBody d

    ppFuncBody :: TypeDecl -> Doc
    ppFuncBody (EnumDecl n s) =
        braces . ppEnumSpec $ s
    ppFuncBody (StructDecl n s) =
        braces . ppStructSpec $ s
    ppFuncBody (UnionDecl n s) =
        braces . ppUnionSpec $ s
    ppFuncBody (SimpleDecl n s) =
        braces (f s <$> ppReturn "TRUE")
      where
        f = ppIfFalseReturn . ppSimpleSpec "xdrs"

    ppEnumSpec :: EnumSpec -> Doc
    ppEnumSpec (EnumSpec cs) =
        (ppIfFalseReturn . ppCallEnum "xdrs" $ "objp")
        <$> ppReturn "TRUE"

    ppStructSpec :: StructSpec -> Doc
    ppStructSpec (StructSpec [d]) =
        f d <$> ppReturn "TRUE"
      where
        f = ppIfFalseReturn . ppStructCall "xdrs"

    ppStructSpec (StructSpec ds) =
        ppXFreeInit
        <$> (vcat . zipWith ppIfFalseGoto [0..] $ allocs)
                <$> ppReturn "TRUE"
                <$> (vcat . map (uncurry ppUnwind)
                              $ drop 1 . reverse . zip [1..] $ frees)
                <$> text "xfree0:"
                <$> ppReturn "FALSE"
      where
        allocs = map (ppStructCall "xdrs") ds
        frees = map (ppStructCall "&xfree") ds

    ppUnionSpec :: UnionSpec -> Doc
    ppUnionSpec (UnionSpec (UnionDis n t) cs md) =
        ppXFreeInit
        <$> (ppIfFalseGoto 0 . ppCallTypeSpec "xdrs" ("&objp->" ++ n) $ t)
                <$> ppSwitch n cs md
                <$> ppReturn "TRUE"
                <$> (ppUnwind 1 $ ppCallTypeSpec "&xfree" ("&objp->" ++ n)
                                  $ t)
                <$> text "xfree0:"
                <$> ppReturn "FALSE"

    ppSwitch n cs md =
        kSwitch <+> (parens . text) ("objp->" ++ n) <+> body
      where
        body = switchBraces . vcat . map (nest indent) $ (xs ++ ys)
        xs = map ppUnionCase cs
        ys = maybeToList . ppUnionDflt $ md

    ppUnionCase :: (ConstExpr, UnionArm) -> Doc
    ppUnionCase (c, d) =
        enclose l r . ppUnionArm $ d
      where
        l = kCase <+> ppConstExpr c <> colon <> line
        r = line <> kBreak <> semi

    ppUnionDflt :: Maybe UnionArm -> Maybe Doc
    ppUnionDflt ma =
        (enclose l r . ppUnionArm) `fmap` ma
      where
        l = kDefault <> char ':'
        r = line <> kBreak <> semi

    ppUnionArm :: UnionArm -> Doc
    ppUnionArm (DeclArm d) =
        ppIfFalseGoto 1 . ppUnionCall "xdrs" $ d
    ppUnionArm VoidArm =
        empty

    ppSimpleSpec :: String -> SimpleSpec -> Doc
    ppSimpleSpec xdrs (PlainSpec t) =
        ppCallTypeSpec xdrs "objp" t
    ppSimpleSpec xdrs (ArraySpec t c) =
        ppCallVector xdrs "objp" c t
    ppSimpleSpec xdrs (VarArraySpec t mc) =
        ppCallArray xdrs "&objp->val" "&objp->len" mc t
    ppSimpleSpec xdrs (OpaqueSpec c) =
        ppCallOpaque xdrs "objp" c
    ppSimpleSpec xdrs (VarOpaqueSpec mc) =
        ppCallBytes xdrs "&objp->val" "&objp->len" mc
    ppSimpleSpec xdrs (StringSpec mc) =
        ppCallString xdrs "objp" mc
    ppSimpleSpec xdrs (PointerSpec t) =
        ppCallPointer xdrs "objp" t

    ppStructCall :: String -> TypeDecl -> Doc
    ppStructCall xdrs (EnumDecl _ _) =
        error "enum not supported"
    ppStructCall xdrs (StructDecl _ _) =
        error "struct not supported"
    ppStructCall xdrs (UnionDecl _ _) =
        error "union not supported"
    ppStructCall xdrs (SimpleDecl n (PlainSpec t)) =
        ppCallTypeSpec xdrs ("&objp->" ++ n) t
    ppStructCall xdrs (SimpleDecl n (ArraySpec t c)) =
        ppCallVector xdrs ("&objp->" ++ n) c t
    ppStructCall xdrs (SimpleDecl n (VarArraySpec t mc)) =
        ppCallArray xdrs ("&objp->" ++ n ++ ".val")
                        ("&objp->" ++ n ++ ".len") mc t
    ppStructCall xdrs (SimpleDecl n (OpaqueSpec c)) =
        ppCallOpaque xdrs ("objp->" ++ n) c
    ppStructCall xdrs (SimpleDecl n (VarOpaqueSpec mc)) =
        ppCallBytes xdrs ("&objp->" ++ n ++ ".val")
                 ("&objp->" ++ n ++ ".len") mc
    ppStructCall xdrs (SimpleDecl n (StringSpec mc)) =
        ppCallString xdrs ("&objp->" ++ n) mc
    ppStructCall xdrs (SimpleDecl n (PointerSpec t)) =
        ppCallPointer xdrs ("&objp->" ++ n) t

    ppUnionCall :: String -> TypeDecl -> Doc
    ppUnionCall xdrs (EnumDecl _ _) =
        error "enum not supported"
    ppUnionCall xdrs (StructDecl _ _) =
        error "struct not supported"
    ppUnionCall xdrs (UnionDecl _ _) =
        error "union not supported"
    ppUnionCall xdrs (SimpleDecl n (PlainSpec t)) =
        ppCallTypeSpec xdrs ("&objp->u." ++ n) t
    ppUnionCall xdrs (SimpleDecl n (ArraySpec t c)) =
        ppCallVector xdrs ("&objp->u." ++ n) c t
    ppUnionCall xdrs (SimpleDecl n (VarArraySpec t mc)) =
        ppCallArray xdrs ("&objp->u." ++ n ++ ".val")
                 ("&objp->u." ++ n ++ ".len") mc t
    ppUnionCall xdrs (SimpleDecl n (OpaqueSpec c)) =
        ppCallOpaque xdrs ("objp->u." ++ n) c
    ppUnionCall xdrs (SimpleDecl n (VarOpaqueSpec mc)) =
        ppCallBytes xdrs ("&objp->u." ++ n ++ ".val")
                        ("&objp->u." ++ n ++ ".len") mc
    ppUnionCall xdrs (SimpleDecl n (StringSpec mc)) =
        ppCallString xdrs ("&objp->u." ++ n) mc
    ppUnionCall xdrs (SimpleDecl n (PointerSpec t)) =
        ppCallPointer xdrs ("&objp->u." ++ n) t
