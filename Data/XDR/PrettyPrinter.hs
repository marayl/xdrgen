module Data.XDR.PrettyPrinter
    ( ppXDR
    ) where

import System.Path
import Text.PrettyPrint.Leijen as PP hiding (braces, indent)

import Data.XDR.AST
import Data.XDR.PPUtils

----------------------------------------------------------------

indent :: Int
indent = 8

ppOptional :: (a -> Doc) -> Maybe a -> Doc
ppOptional _ Nothing = empty
ppOptional fn (Just a) = fn a

braces :: [Doc] -> Doc
braces ds = nest indent (lbrace <$> vcat ds) <$> rbrace

-- | Pretty print an AST back to XDR format.  FIXME: The first
--   argument is ignored and will be disappearing soon.
ppXDR :: ModuleSpec -> String
ppXDR = show . ppSpec

-- | FIXME: print the imports
ppSpec :: ModuleSpec -> Doc
ppSpec (ModuleSpec _ _ ds) = vcat . punctuate linebreak . map ppDef $ ds

ppDef :: Definition -> Doc
ppDef (TypeDef td) = text "typedef" <+> ppTypeDecl td <> semi
ppDef (ConstDef cd) = ppConstDecl cd

ppTypeDecl :: TypeDecl -> Doc
ppTypeDecl (EnumDecl n s) = text "enum" <+> ppEnumSpec s <+> text n
ppTypeDecl (StructDecl n s) = text "struct" <+> ppStructSpec s <+> text n
ppTypeDecl (UnionDecl n s) = text "union" <+> ppUnionSpec s <+> text n
ppTypeDecl (SimpleDecl n s) = ppSimpleSpec n s

ppConstDecl :: ConstDecl -> Doc
ppConstDecl (ConstDecl n c) = text "const" <+> text n <+> text "=" <+> ppConstExpr c <> semi

ppEnumSpec :: EnumSpec -> Doc
ppEnumSpec (EnumSpec cs) = braces . punctuate comma . map ppEnumConstDecl $ cs

ppEnumConstDecl :: ConstDecl -> Doc
ppEnumConstDecl (ConstDecl n c) = text n <+> text "=" <+> ppConstExpr c

ppStructSpec :: StructSpec -> Doc
ppStructSpec (StructSpec ds) = braces . map ((<> semi) . ppTypeDecl) $ ds

ppUnionSpec :: UnionSpec -> Doc
ppUnionSpec (UnionSpec (UnionDis n t) cs md) =
    vcat $ concat [ [ text "switch" <> parens (ppTypeSpec t <+> text n) <+> lbrace ]
                  , map ppUnionCase cs
                  , [ppUnionDflt md]
                  , [rbrace]
                  ]

ppUnionCase :: (ConstExpr, UnionArm) -> Doc
ppUnionCase (c, d) = nest indent (text "case" <+> ppConstExpr c <> colon <$> ppUnionArm d <> semi)

ppUnionDflt :: Maybe UnionArm -> Doc
ppUnionDflt d = ppOptional (\d -> nest indent (text "default:" <$> ppUnionArm d <> semi)) d

ppUnionArm :: UnionArm -> Doc
ppUnionArm (DeclArm d) = ppTypeDecl d
ppUnionArm VoidArm = text "void"

ppSimpleSpec :: String -> SimpleSpec -> Doc
ppSimpleSpec n (PlainSpec t) =
    ppTypeSpec t <+> text n
ppSimpleSpec n (ArraySpec t c) =
    ppTypeSpec t <+> text n <> (brackets . ppConstExpr $ c)
ppSimpleSpec n (VarArraySpec t mc) =
    ppTypeSpec t <+> text n <+> ppVarSize mc
ppSimpleSpec n (OpaqueSpec c) =
    text "opaque" <+> text n <> (brackets . ppConstExpr $ c)
ppSimpleSpec n (VarOpaqueSpec mc) =
    text "opaque" <+> text n <> ppVarSize mc
ppSimpleSpec n (StringSpec mc) =
    text "string" <+> text n <> ppVarSize mc
ppSimpleSpec n (PointerSpec t) =
    ppTypeSpec t <> text "*" <+> text n

ppVarSize :: Maybe ConstExpr -> Doc
ppVarSize = angles . ppOptional ppConstExpr

ppTypeSpec :: TypeSpec -> Doc
ppTypeSpec IntSpec = text "int"
ppTypeSpec UIntSpec = text "unsigned int"
ppTypeSpec HyperSpec = text "hyper"
ppTypeSpec UHyperSpec = text "unsigned hyper"
ppTypeSpec FloatSpec = text "float"
ppTypeSpec DoubleSpec = text "double"
ppTypeSpec QuadrupleSpec = text "quadruple"
ppTypeSpec BoolSpec = text "bool"
ppTypeSpec (NamedSpec n) = text n

----------------------------------------------------------------
