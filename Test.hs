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

testMultiplication =
    TestCase (assertEval 77 "7 * 11"
              >> assertEval 3 "33 / 11"
              >> assertEval 4 "11 % 7")

testAddition =
    TestCase (assertEval 18 "7 + 11"
              >> assertEval (-6) "7 - 13")

testShift =
    TestCase (assertEval 64 "16 << 2"
              >> assertEval 4 "16 >> 2")

testLogical =
    TestCase (assertEval 15 "0xfff & 0x00f"
              >> assertEval 15 "0xff0 ^ 0xfff"
              >> assertEval 15 "0x00f | 0x00f")

testUnary =
    TestCase (assertEval (-1) "-1"
              >> assertEval (-1) "~0")

testDistributive =
    TestCase (assertEval 14 "2 * (3 + 4)"
              >> assertEval 14 "2 * 3 + 2 * 4)"
             )

testEnum =
    TestCase (assertDef
              (TypeDef (EnumDecl "x"
                        (EnumSpec [ ConstDecl "A" (LitExpr 0)
                                  , ConstDecl "B" (LitExpr 1)
                                  , ConstDecl "C" (LitExpr 2)
                                  ]
                        )
                       )
              ) (parse "enum x { A, B, C };")

              >> assertDef
              (TypeDef (EnumDecl "x"
                        (EnumSpec [ ConstDecl "A" (UnExpr NEG (LitExpr 1))
                                  , ConstDecl "B" (LitExpr 0)
                                  , ConstDecl "C" (LitExpr 1)
                                  ]
                        )
                       )
              ) (parse "enum x { A = -1, B, C };")

              >> assertDef
              (TypeDef (EnumDecl "x"
                        (EnumSpec [ ConstDecl "A"
                                    (NamedExpr (ConstDecl "X" (LitExpr 1)))
                                  , ConstDecl "B" (LitExpr 2)
                                  ]
                        )
                       )
              ) (parse "const X = 1; typedef enum { A = X, B } x;")
             )

testStruct =
    TestCase (assertDef
              (TypeDef (StructDecl "x"
                        (StructSpec [ SimpleDecl "i" (PlainSpec IntSpec)])
                       )
              ) (parse "struct x { int i; };")
             )

testUnion =
    TestCase (assertDef
              (TypeDef (UnionDecl "x"
                        (UnionSpec (UnionDis "opt" BoolSpec)
                         [ (LitExpr 1,
                            DeclArm (SimpleDecl "i" (PlainSpec IntSpec)))
                         ] Nothing
                        )
                       )
              ) (parse "union x switch (bool opt) { case TRUE: int i; };")
             )

testTypedef =
    TestCase (assertDef
              (TypeDef (EnumDecl "x"
                        (EnumSpec [ ConstDecl "A" (LitExpr 255)
                                  , ConstDecl "B" (LitExpr 256)
                                  ]
                        )
                       )
              ) (parse "typedef enum { A = 0xff, B } x;")
              >> assertDef
              (TypeDef (StructDecl "x"
                        (StructSpec [ SimpleDecl "i" (PlainSpec IntSpec)])
                       )
              ) (parse "typedef struct { int i; } x;")
              >> assertDef
              (TypeDef (UnionDecl "x"
                        (UnionSpec (UnionDis "opt" BoolSpec)
                         [ (LitExpr 1,
                            DeclArm (SimpleDecl "i" (PlainSpec IntSpec)))
                         ] Nothing
                        )
                       )
              ) (parse "typedef union switch (bool opt) { case TRUE: int i; } x;")
             )

testString =
    TestCase (assertDef
              (TypeDef
               (SimpleDecl "x" (StringSpec Nothing))
              ) (parse "typedef string x<>;")
              >> assertDef
              (TypeDef
               (SimpleDecl "x" (StringSpec (Just (LitExpr 7))))
              ) (parse "typedef string x<7>;")
             )

testOpaque =
    TestCase (assertDef
              (TypeDef
               (SimpleDecl "x" (OpaqueSpec (LitExpr 7)))
              ) (parse "typedef opaque x[7];")
              >> assertDef
              (TypeDef
               (SimpleDecl "x" (VarOpaqueSpec Nothing))
              ) (parse "typedef opaque x<>;")
              >> assertDef
              (TypeDef
               (SimpleDecl "x" (VarOpaqueSpec (Just (LitExpr 7))))
              ) (parse "typedef opaque x<7>;")
             )

testArray =
    TestCase (assertDef
              (TypeDef
               (SimpleDecl "x" (ArraySpec IntSpec (LitExpr 7)))
              ) (parse "typedef int x[7];")
              >> assertDef
              (TypeDef
               (SimpleDecl "x" (VarArraySpec IntSpec Nothing))
              ) (parse "typedef int x<>;")
              >> assertDef
              (TypeDef
               (SimpleDecl "x" (VarArraySpec IntSpec (Just (LitExpr 7))))
              ) (parse "typedef int x<7>;")
             )

tests =
    TestList [ TestLabel "constant decimal" testConstDec
             , TestLabel "constant hexadecimal" testConstHex
             , TestLabel "multiplication expression" testMultiplication
             , TestLabel "addition expression" testAddition
             , TestLabel "shift expression" testShift
             , TestLabel "logical expression" testLogical
             , TestLabel "unary expression" testUnary
             , TestLabel "distributive" testDistributive
             , TestLabel "enum declaration" testEnum
             , TestLabel "struct declaration" testStruct
             , TestLabel "union declaration" testUnion
             , TestLabel "typedef declaration" testTypedef
             , TestLabel "typedef string" testString
             , TestLabel "typedef opaque" testOpaque
             , TestLabel "typedef array" testArray
            ]

main =
    runTestTT tests
