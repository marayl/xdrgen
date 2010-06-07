module Data.XDR.PPUtils
       ( ppConstExpr
       ) where


import Data.XDR.AST
import Text.PrettyPrint.Leijen as PP hiding (braces, indent)

ppConstExpr :: ConstExpr -> Doc
ppConstExpr (LitExpr n) = text . show $ n
ppConstExpr (NamedExpr (ConstDecl n _)) = text n
ppConstExpr (BinExpr op c1 c2) = ppBinOp op (ppConstExpr c1) (ppConstExpr c2)
ppConstExpr (UnExpr NEG c) = parens $ text "-" <> ppConstExpr c
ppConstExpr (UnExpr NOT c) = parens $ text "~" <> ppConstExpr c

ppBinOp :: BinOp -> Doc -> Doc -> Doc
ppBinOp op d1 d2 = parens $ parens d1 </> (text . symbol $ op) <+> parens d2
        where
          symbol MUL = "*"
          symbol DIV = "/"
          symbol MOD = "%"
          symbol ADD = "+"
          symbol SUB = "-"
          symbol SHR = ">>"
          symbol SHL = "<<"
          symbol AND = "&"
          symbol XOR = "^"
          symbol OR = "|"
