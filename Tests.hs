import Control.Monad
import Data.ByteString.UTF8
import Data.XDR.AST
import Data.XDR.Parser

import Test.HUnit

parse :: String -> Either [ParseError] ModuleSpec
parse s = parseString [] (fromString s) $ "test"

assertEval :: Integer -> String -> Assertion
assertEval expected s =
    case parseConstExpr (fromString s) "test" of
      Left err -> assertFailure . show $ err
      Right c -> assertEqual "" expected (evalConstExpr c)

assertErr :: Either [ParseError] ModuleSpec -> Assertion
assertErr r =
    unless (either (\x -> True) (\x -> False) r)
               (assertFailure "expected error")

assertDef :: Definition -> Either [ParseError] ModuleSpec -> Assertion
assertDef _ (Left err) =
    assertFailure . show $ err
assertDef expected (Right (ModuleSpec _ _ [])) =
    assertFailure "no definitions"
assertDef expected (Right (ModuleSpec _ _ actual)) =
    assertEqual "" expected (last actual)

testConstDec =
    TestCase (assertDef
              (ConstDef (ConstDecl "X" (LitExpr 255)))
              (parse "const X = 255;"))

testConstHex =
    TestCase (assertDef
              (ConstDef (ConstDecl "X" (LitExpr 255)))
              (parse "const X = 0xff;"))

testConstMul =
    TestCase . assertEval 77 $ "7 * 11"

testConstDiv =
    TestCase . assertEval 3 $ "33 / 11"

testConstMod =
    TestCase . assertEval 4 $ "11 % 7"

testConstAdd =
    TestCase . assertEval 18 $ "7 + 11"

testConstSub =
    TestCase . assertEval (-6) $ "7 - 13"

testConstShl =
    TestCase . assertEval 64 $ "16 << 2"

testConstShr =
    TestCase . assertEval 4 $ "16 >> 2"

testConstAnd =
    TestCase . assertEval 15 $ "0xfff & 0x00f"

testConstXor =
    TestCase . assertEval 15 $ "0xff0 ^ 0xfff"

testConstOr =
    TestCase . assertEval 15 $ "0x00f | 0x00f"

testConstNeg =
    TestCase . assertEval (-1) $ "-1"

testConstNot =
    TestCase . assertEval (-1) $ "~0"

tests =
    TestList [ TestLabel "constant decimal" testConstDec
             , TestLabel "constant hexadecimal" testConstHex
             , TestLabel "constant multiplication" testConstMul
             , TestLabel "constant division" testConstDiv
             , TestLabel "constant modulus" testConstMod
             , TestLabel "constant addition" testConstAdd
             , TestLabel "constant subtraction" testConstSub
             , TestLabel "constant shift left" testConstShl
             , TestLabel "constant shift right" testConstShr
             , TestLabel "constant logical and" testConstAnd
             , TestLabel "constant logical xor" testConstXor
             , TestLabel "constant logical or" testConstOr
             , TestLabel "constant negation" testConstNeg
             , TestLabel "constant complement" testConstNot
             ]

main =
    runTestTT tests
