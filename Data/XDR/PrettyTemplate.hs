module Data.XDR.PrettyTemplate
    ( ppTemplate
    ) where

import Text.PrettyPrint.Leijen as PP hiding (braces, indent)

import Data.XDR.AST
import Data.XDR.PPUtils

ppTemplate :: ModuleSpec -> String
ppTemplate = show . ppSpec

ppSpec :: ModuleSpec -> Doc
ppSpec (ModuleSpec _ _ ds) =
    vcat . punctuate linebreak . map ppDef $ ds

ppDef :: Definition -> Doc
ppDef (TypeDef td) =
    ppTypeDecl td
ppDef (ConstDef cd) =
    ppConstDecl cd

ppTypeDecl :: TypeDecl -> Doc
ppTypeDecl (EnumDecl n s) =
    ppEnumSpec s
ppTypeDecl (StructDecl n s) =
    ppStructSpec s
ppTypeDecl (UnionDecl n s) =
    ppUnionSpec s
ppTypeDecl (SimpleDecl n s) =
    ppSimpleSpec n s

ppConstDecl :: ConstDecl -> Doc
ppConstDecl (ConstDecl n c) =
    undefined

ppEnumSpec :: EnumSpec -> Doc
ppEnumSpec (EnumSpec cs) =
    undefined

ppEnumConstDecl :: ConstDecl -> Doc
ppEnumConstDecl (ConstDecl n c) =
    undefined

ppStructSpec :: StructSpec -> Doc
ppStructSpec (StructSpec ds) =
    undefined

ppUnionSpec :: UnionSpec -> Doc
ppUnionSpec (UnionSpec (UnionDis n t) cs md) =
    undefined

ppUnionCase :: (ConstExpr, UnionArm) -> Doc
ppUnionCase (c, d) =
    undefined

ppUnionDflt :: Maybe UnionArm -> Doc
ppUnionDflt d =
    undefined

ppUnionArm :: UnionArm -> Doc
ppUnionArm (DeclArm d) =
    ppTypeDecl d
ppUnionArm VoidArm =
    text "void"

ppSimpleSpec :: String -> SimpleSpec -> Doc
ppSimpleSpec n (PlainSpec t) =
    undefined
ppSimpleSpec n (ArraySpec t c) =
    undefined
ppSimpleSpec n (VarArraySpec t mc) =
    undefined
ppSimpleSpec n (OpaqueSpec c) =
    undefined
ppSimpleSpec n (VarOpaqueSpec mc) =
    undefined
ppSimpleSpec n (StringSpec mc) =
    undefined
ppSimpleSpec n (PointerSpec t) =
    undefined

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
