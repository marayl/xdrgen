module Data.XDR.PrettyPrintJava
    ( ppJava
    ) where

import Control.Monad
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.XDR.AST
import Data.XDR.PPKeywords
import Data.XDR.PPUtils
import Text.PrettyPrint.Leijen as PP hiding (braces, indent)

-- | Bird.

pair :: (a -> b, a -> c) -> a -> (b, c)
pair (f, g) x = (f x, g x)

-- | Bird.

cross :: (a -> b, c -> d) -> (a, c) -> (b, d)
cross (f, g) = pair (f . fst, g . snd)

indent :: Int
indent = 4

lrparen :: Doc
lrparen = lparen <> rparen

braces :: Doc -> Doc
braces d = nest indent (lbrace <$> d) <$> rbrace

maybePush :: Maybe a -> [a] -> [a]
maybePush =
    maybe id (:)

camelCase :: String -> String
camelCase [] = []
camelCase ('_':x:xs) = toUpper x : camelCase xs
camelCase (x:xs) = x : camelCase xs

typeName :: String -> String
typeName [] = []
typeName (x:xs) = toUpper x : camelCase xs

topName :: ModuleName -> String
topName (ModuleName xs) =
    last xs

upperName :: String -> String
upperName = map toUpper

type DeclMap = Map String TypeDecl

lookupDecl :: String -> DeclMap -> TypeDecl
lookupDecl n m =
    case M.lookup n m of
      Just d -> d
      Nothing -> error ("unknown type: " ++ n)

specToDeclMap :: ModuleSpec -> DeclMap -> DeclMap
specToDeclMap (ModuleSpec _ imports defs) m =
    defsToDeclMap defs m'
  where
    m' = foldr specToDeclMap m (M.elems imports)

armToDecl :: UnionArm -> Maybe TypeDecl
armToDecl (DeclArm d) = Just d
armToDecl VoidArm = Nothing

defsToDeclMap :: [Definition] -> DeclMap -> DeclMap
defsToDeclMap =
    flip (foldr f)
  where
    f (TypeDef td) =
        M.insert (declName td) td
    f _ = id

-- JType

data JType = JType
    { jType :: Doc
    , jUnbox :: Doc
    , jCodec :: Doc
    , jConstExpr :: ConstExpr -> Doc
    }

jTypeDecl :: TypeDecl -> DeclMap -> JType
jTypeDecl (EnumDecl n s) m =
    jTypeSpec IntSpec m
jTypeDecl d@(StructDecl n _) m =
    JType td td cd ppConstExpr
  where
    td = text $ typeName n
    cd = ppTypeDeclCodec d
jTypeDecl d@(UnionDecl n (UnionSpec (UnionDis _ t) _ _)) m =
    JType td td cd ppConstExpr
 where
    td = text "Union" <> langle <> (jType . jTypeSpec t $ m) <> rangle
    cd = ppTypeDeclCodec d
jTypeDecl (SimpleDecl n s) m =
    jSimpleSpec s m

jSimpleSpec :: SimpleSpec -> DeclMap -> JType
jSimpleSpec (PlainSpec t) m =
    jTypeSpec t m
jSimpleSpec s@(ArraySpec t _) m =
    JType td td cd ppConstExpr
  where
    td = text "Array" <> langle <> jType jt <> rangle
    cd = ppSimpleSpecCodec s
    jt = jTypeSpec t m
jSimpleSpec s@(VarArraySpec t _) m =
    JType td td cd ppConstExpr
  where
    td = text "Array" <> langle <> jType jt <> rangle
    cd = ppSimpleSpecCodec s
    jt = jTypeSpec t m
jSimpleSpec s@(OpaqueSpec _) _ =
    JType td td cd ppConstExpr
  where
    td = text "Opaque"
    cd = ppSimpleSpecCodec s
jSimpleSpec s@(VarOpaqueSpec _) m =
    JType td td cd ppConstExpr
  where
    td = text "Opaque"
    cd = ppSimpleSpecCodec s
jSimpleSpec s@(StringSpec _) m =
    JType td td cd ppConstExpr
  where
    td = text "String"
    cd = ppSimpleSpecCodec s
jSimpleSpec s@(PointerSpec t) m =
    JType td td cd fn
  where
    JType td _ _ fn = jTypeSpec t m
    cd = ppSimpleSpecCodec s

jTypeSpec :: TypeSpec -> DeclMap -> JType
jTypeSpec IntSpec _ =
    JType (text "Integer") kInt cd ppConstExpr
  where
    cd = ppTypeSpecCodec IntSpec
jTypeSpec UIntSpec _ =
    JType (text "Integer") kInt cd ppConstExpr
  where
    cd = ppTypeSpecCodec UIntSpec
jTypeSpec HyperSpec _ =
    JType (text "Long") kLong cd ppConstExpr
  where
    cd = ppTypeSpecCodec HyperSpec
jTypeSpec UHyperSpec _ =
    JType (text "Long") kLong cd ppConstExpr
  where
    cd = ppTypeSpecCodec UHyperSpec
jTypeSpec FloatSpec _ =
    JType (text "Float") kFloat cd ppConstExpr
  where
    cd = ppTypeSpecCodec FloatSpec
jTypeSpec DoubleSpec _ =
    JType (text "Double") kDouble cd ppConstExpr
  where
    cd = ppTypeSpecCodec DoubleSpec
jTypeSpec QuadrupleSpec _ =
    error "quadruple not supported"
jTypeSpec BoolSpec _ =
    JType (text "Boolean") kBoolean cd f
  where
    cd = ppTypeSpecCodec BoolSpec
    f = (text "0 !=" <+>) . ppConstExpr
-- | Resolve name from decl map.
jTypeSpec (NamedSpec n) m =
    JType td ud cd fn
  where
    JType td ud _ fn = flip jTypeDecl m . lookupDecl n $ m
    cd = text ("Xdr" ++ typeName n ++ ".CODEC")

ppTypeDeclCodec :: TypeDecl -> Doc
-- | Enumerations map to native integers.
ppTypeDeclCodec (EnumDecl _ _) =
    ppTypeSpecCodec IntSpec
ppTypeDeclCodec (StructDecl n _) =
    text ("Xdr" ++ typeName n ++ ".CODEC")
ppTypeDeclCodec (UnionDecl _ (UnionSpec (UnionDis _ t) _ md)) =
    text "XdrUnion.newCodec"
             <> tupled (dis : text "CASES" : maybeToList dfl)
  where
    dis = ppTypeSpecCodec t
    dfl = md >>= ppUnionArmCodec

ppTypeDeclCodec (SimpleDecl _ s) =
    ppSimpleSpecCodec s

ppUnionArmCodec :: UnionArm -> Maybe Doc
ppUnionArmCodec a = ppTypeDeclCodec `fmap` armToDecl a

ppSimpleSpecCodec :: SimpleSpec -> Doc
ppSimpleSpecCodec (PlainSpec t) =
    ppTypeSpecCodec t
ppSimpleSpecCodec (ArraySpec t c) =
    text "XdrArray.newCodec"
             <> tupled [ppTypeSpecCodec t, ppConstExpr c]
ppSimpleSpecCodec (VarArraySpec t mc) =
    text "XdrArray.newVarCodec"
             <> tupled [ppTypeSpecCodec t,
                        maybe (text "Integer.MAX_VALUE") ppConstExpr mc]
ppSimpleSpecCodec (OpaqueSpec c) =
    text "XdrOpaque.newCodec" <> tupled [ppConstExpr c]
ppSimpleSpecCodec (VarOpaqueSpec mc) =
    text "XdrOpaque." <> maybe (text "VAR_CODEC") ppVarCodec mc
ppSimpleSpecCodec (StringSpec mc) =
    text "XdrString." <> maybe (text "VAR_CODEC") ppVarCodec mc
ppSimpleSpecCodec (PointerSpec t) =
    text "XdrOptional.newCodec" <> tupled [ppTypeSpecCodec t]

ppVarCodec :: ConstExpr -> Doc
ppVarCodec c =
    text "newVarCodec" <> tupled [ppConstExpr c]

ppTypeSpecCodec :: TypeSpec -> Doc
ppTypeSpecCodec IntSpec = text "XdrInt.CODEC"
ppTypeSpecCodec UIntSpec = text "XdrUInt.CODEC"
ppTypeSpecCodec HyperSpec = text "XdrHyper.CODEC"
ppTypeSpecCodec UHyperSpec = text "XdrUHyper.CODEC"
ppTypeSpecCodec FloatSpec = text "XdrFloat.CODEC"
ppTypeSpecCodec DoubleSpec = text "XdrDouble.CODEC"
ppTypeSpecCodec QuadrupleSpec = error "not supported"
ppTypeSpecCodec BoolSpec = text "XdrBool.CODEC"
ppTypeSpecCodec (NamedSpec n) = text ("Xdr" ++ typeName n ++ ".CODEC")

-- Type-names and keywords.

kByteBuffer = text "java.nio.ByteBuffer"
kCharacterCodingException = text "java.nio.charset.CharacterCodingException"

ppMaybePackage :: ModuleName -> Maybe Doc
ppMaybePackage (ModuleName xs) =
    if null ps then Nothing
    else Just $ kPackage <+> f ps <> semi
  where
    f = text . concat . intersperse "."
    ps = init xs

ppImports :: [ModuleName] -> Doc
ppImports =
    (kImport <+> text "org.openxdr.*" <> semi <$>) . vcat . map ppImport

ppImport :: ModuleName -> Doc
ppImport m@(ModuleName xs) =
    kImport <+> kStatic <+> f xs <> text ".*" <> semi
  where
    f = text . concat . intersperse "."

ppClass :: String -> [Doc] -> Doc
ppClass n =
    (head <+>) . braces . vcat . (ppPrivateCons n :)
  where
    head = kClass <+> text n

ppPrivateCons :: String -> Doc
ppPrivateCons n =
    kPrivate <+> text n <> lrparen <+> lbrace <$> rbrace

ppStaticCodec :: String -> Doc -> Doc -> Doc
ppStaticCodec n t d =
    nest indent (decl <+> equals </> d) <> semi
  where
    decl = kStatic <+> kFinal <+> text "Codec"
           <> langle <> t <> rangle <+> text n

ppIface :: String -> [(String, JType)] -> Doc
ppIface n =
    (head <+>) . braces . vcat . map ppGetterDecl
  where
    head = kInterface <+> text n

ppGetterName :: String -> Doc
ppGetterName n =
    text ("get" ++ typeName n)

ppGetterDecl :: (String, JType) -> Doc
ppGetterDecl (n, jt) =
    jUnbox jt <+> ppGetterName n <> lrparen <> semi

ppFactory :: String -> [(String, JType)] -> Doc
ppFactory n jts =
    (head <+>) . braces $ ppInstance n jts
  where
    head = kStatic <+> kFinal <+> text n <+> text ("new" ++ n)
           <> tupled (ppFactoryParams jts)

ppInstance :: String -> [(String, JType)] -> Doc
ppInstance n =
    (<> semi) . (head <+>) . braces . vcat . ppGetterImpls
  where
    head = kReturn <+> kNew <+> text n <> lrparen

ppGetterImpls :: [(String, JType)] -> [Doc]
ppGetterImpls =
    map ppGetterImpl

ppGetterImpl :: (String, JType) -> Doc
ppGetterImpl (n, jt) =
    kPublic <+> kFinal <+> jUnbox jt
                <+> ppGetterName n <> lrparen
                <+> braces body
  where
    body = kReturn <+> text (cn ++ "_") <> semi
    cn = camelCase n

ppFactoryParams :: [(String, JType)] -> [Doc]
ppFactoryParams =
    map ppFactoryParam

ppFactoryParam :: (String, JType) -> Doc
ppFactoryParam (n, jt) =
    kFinal <+> jUnbox jt <+> text (cn ++ "_")
  where
    cn = camelCase n

ppFactoryCall :: String -> [(String, JType)] -> Doc
ppFactoryCall n =
    (head <>) . (<> semi) . tupled . ppFactoryArgs
  where
    head = kReturn <+> text ("new" ++ tn)
    tn = typeName n

ppFactoryArgs :: [(String, JType)] -> [Doc]
ppFactoryArgs =
    map ppFactoryArg

ppFactoryArg :: (String, JType) -> Doc
ppFactoryArg (n, _) =
    text (cn ++ "_")
  where
    cn = camelCase n

ppAnonCodec :: String -> [(String, JType)] -> Doc
ppAnonCodec n jts =
    nest indent (head <+> braces body) <> semi
  where
    head = kPublic <+> kStatic <+> kFinal <+> ct <+> text "CODEC" <+> equals
           </> kNew <+> ct <> lrparen
    body = ppEncode n jts <$> ppDecode n jts <$> ppSize n jts
    ct = text "Codec" <> langle <> text tn <> rangle
    tn = typeName n

ppEncode :: String -> [(String, JType)] -> Doc
ppEncode n =
    (nest indent head <+>) . braces . ppEncodeBody
  where
    head = kPublic <+> kFinal <+> kVoid <+> text "encode"
           <> tupled [kByteBuffer <+> text "buf", text (tn ++ " val")]
           </> kThrows <+> kCharacterCodingException
    tn = typeName n

ppEncodeBody :: [(String, JType)] -> Doc
ppEncodeBody =
    vcat . ppEncodeVars

ppEncodeVars :: [(String, JType)] -> [Doc]
ppEncodeVars =
    map ppEncodeVar

ppEncodeVar :: (String, JType) -> Doc
ppEncodeVar (n, jt) =
    text un <> text "_CODEC.encode"
             <> tupled [text "buf", text ("val.get" ++ tn) <> lrparen] <> semi
  where
    tn = typeName n
    un = upperName n

ppDecode :: String -> [(String, JType)] -> Doc
ppDecode n =
    (nest indent head <+>) . braces . ppDecodeBody n
  where
    head = kPublic <+> kFinal <+> text tn <+> text "decode"
           <> tupled [kByteBuffer <+> text "buf"]
           </> kThrows <+> kCharacterCodingException
    tn = typeName n

ppDecodeBody :: String -> [(String, JType)] -> Doc
ppDecodeBody n jts =
    (vcat $ ppDecodeVars jts) <$> ppFactoryCall n jts

ppDecodeVars :: [(String, JType)] -> [Doc]
ppDecodeVars =
    map ppDecodeVar

ppDecodeVar :: (String, JType) -> Doc
ppDecodeVar (n, jt) =
    kFinal <+> jUnbox jt <+> text (cn ++ "_") <+> equals
               <+> text un <> text "_CODEC.decode"
               <> tupled [text "buf"] <> semi
  where
    cn = camelCase n
    un = upperName n

ppSize :: String -> [(String, JType)] -> Doc
ppSize n =
    (nest indent head <+>) . braces . ppSizeBody
  where
    head = kPublic <+> kFinal <+> kInt <+> text "size"
           <> tupled [text (tn ++ " val")]
    tn = typeName n

ppSizeBody :: [(String, JType)] -> Doc
ppSizeBody jts =
    kInt <+> text "n" <+> equals <+> text "0" <> semi
              <$> (vcat $ ppSizeVars jts)
              <$> kReturn <+> text "n" <> semi

ppSizeVars :: [(String, JType)] -> [Doc]
ppSizeVars =
    map ppSizeVar

ppSizeVar :: (String, JType) -> Doc
ppSizeVar (n, jt) =
    text "n +=" <+> text un <> text "_CODEC.size"
             <> tupled [text ("val.get" ++ tn) <> lrparen] <> semi
  where
    tn = typeName n
    un = upperName n

ppJava :: ModuleSpec -> String
ppJava spec =
    show . vcat $ maybePush (ppMaybePackage m)
             [ppImports . M.keys $ moduleImports spec, body]
  where
    body = kPublic <+> kFinal <+> ppClass tn (ppSpec spec)
    tn = topName m
    m = moduleName spec

ppSpec :: ModuleSpec -> [Doc]
ppSpec s@(ModuleSpec _ _ ds) =
    f ds
  where
    f = punctuate linebreak . map (flip ppDef m)
    m = specToDeclMap s M.empty

ppDef :: Definition -> DeclMap -> Doc
ppDef (TypeDef td) m =
    ppTypeDecl td m
ppDef (ConstDef cd) m =
    ppConstDecl cd

ppTypeDecl :: TypeDecl -> DeclMap -> Doc

-- | Enumerated values must be visible in the top-level namespace, so they are
-- not enclosed in a separate class or interface.
ppTypeDecl d@(EnumDecl n (EnumSpec cs)) m =
    (vcat . map ppConstDecl $ cs) <$> (ppSimpleCodec d m)

ppTypeDecl (StructDecl n (StructSpec ds)) m =
    kPublic <+> ppIface tn jts
                <$> kPublic <+> ppFactory tn jts
                <$> kPublic <+> kStatic <+> kFinal
                <+> ppClass ("Xdr" ++ tn) (anc : cds)
    where
      anc = ppAnonCodec n jts
      cds = map (flip ppPrivateCodec m) ds
      jts = map (pair (declName, flip jTypeDecl m)) ds
      tn = typeName n

ppTypeDecl d@(UnionDecl n s) m =
    kPublic <+> kStatic <+> kFinal
                <+> ppClass ("Xdr" ++ typeName n)
                        [ppUnionCases s m, ppPublicCodec d m]

ppTypeDecl d@(SimpleDecl _ _) m =
    ppSimpleCodec d m

ppConstDecl :: ConstDecl -> Doc
ppConstDecl (ConstDecl n c) =
    kPublic <+> kStatic <+> kFinal <+> kInt <+> text n'
                <+> equals <+> nest indent (ppConstExpr c) <> semi
  where
    n' = upperName n

ppUnionCases :: UnionSpec -> DeclMap -> Doc
ppUnionCases (UnionSpec (UnionDis n t) cs md) m =
    nest indent head <> semi
  where
    head = kPrivate <+> kStatic <+> kFinal <+> text "java.util.Map"
           <> langle <> jType jt <> char ','
           <+> text "Codec" <> langle <> char '?' <> rangle <> rangle
           <+> text "CASES" <+> equals </> text "XdrUnion.newCases"
           <> tupled (ppUnionPairs m (jConstExpr jt) cs)
    jt = jTypeSpec t m

ppUnionPairs :: DeclMap -> (ConstExpr -> Doc) -> [(ConstExpr, UnionArm)]
             -> [Doc]
ppUnionPairs m f =
    foldr (g . ppUnionPair m f) []
  where
    g (c, d) = (c :) . (d :)

ppUnionPair :: DeclMap -> (ConstExpr -> Doc) -> (ConstExpr, UnionArm)
            -> (Doc, Doc)
ppUnionPair m f (c, d) =
    (f c, ppUnionArm d)

ppUnionCase :: (ConstExpr, UnionArm) -> Doc
ppUnionCase (c, d) =
    undefined

ppUnionDflt :: Maybe UnionArm -> Doc
ppUnionDflt d =
    undefined

ppUnionArm :: UnionArm -> Doc
ppUnionArm (DeclArm d) =
    ppTypeDeclCodec d
ppUnionArm VoidArm =
    text "XdrVoid.CODEC"

ppSimpleCodec :: TypeDecl -> DeclMap -> Doc
ppSimpleCodec d m =
    kPublic <+> kStatic <+> kFinal
                <+> ppClass ("Xdr" ++ (typeName . declName $ d))
                        [ppPublicCodec d m]

ppPublicCodec :: TypeDecl -> DeclMap -> Doc
ppPublicCodec d m =
    (kPublic <+>) . ppStaticCodec "CODEC" td $ cd
  where
    td = jType jt
    cd = jCodec jt
    jt = jTypeDecl d m

ppPrivateCodec :: TypeDecl -> DeclMap -> Doc
ppPrivateCodec d m =
    (kPrivate <+>) . ppStaticCodec (upperName n ++ "_CODEC") td $ cd
  where
    n = declName d
    td = jType jt
    cd = jCodec jt
    jt = jTypeDecl d m
