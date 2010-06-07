{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, FlexibleInstances #-}
-- | Data types for defining the abstract syntax tree of an XDR file.
module Data.XDR.AST
    (
      -- * Constant expressions
      ConstExpr (..)
    , BinOp (..)
    , UnOp (..)
    , evalConstExpr

      -- * XDR types
    , ConstDecl (..)
    , ModuleSpec (..)
    , ModuleName (..)
    , Definition (..)
    , TypeDecl (..)
    , EnumSpec (..)
    , StructSpec (..)
    , UnionSpec (..)
    , UnionDis (..)
    , UnionArm (..)
    , SimpleSpec (..)
    , TypeSpec (..)
    , declName
    , typeDecls
    ) where

import Data.Bits
import Data.Generics
import Data.Map (Map)
import Data.Maybe
import System.Path

-- | Constants are represented as symbolic expressions to allow the code
--   generators to produce more tractable code.  Utility functions can
--   evaluate these expressions.  A primary expression is just a reference to
--   a literal, global constant or previously defined enum element.

data ConstExpr = LitExpr Integer
               | NamedExpr ConstDecl
               | BinExpr BinOp ConstExpr ConstExpr
               | UnExpr UnOp ConstExpr
                 deriving (Show, Typeable, Data)

data BinOp = MUL | DIV | MOD | ADD | SUB | SHL | SHR | AND | XOR | OR
             deriving (Eq, Show, Typeable, Data)

data UnOp = NEG | NOT
            deriving (Eq, Show, Typeable, Data)

data ConstDecl = ConstDecl String ConstExpr
                 deriving (Show, Typeable, Data)

data ModuleSpec = ModuleSpec {
      moduleName :: ModuleName

    -- | A map of other xdr files that have been imported.  Empty if the
    -- 'Data.XDR.Parser.Imports' language option was not enabled.
    , moduleImports :: Map ModuleName ModuleSpec

    -- | The data type definitions.
    , moduleDefs :: [Definition]
    } deriving Show

-- | The module name as list of elements.
newtype ModuleName = ModuleName [String]
    deriving (Eq, Ord, Show, Typeable, Data)

-- | A statement either introduces a new type, or defines a constant.  A
-- typedef associates a name with a type.
data Definition = TypeDef TypeDecl
                | ConstDef ConstDecl
                  deriving (Show, Typeable, Data)

data TypeDecl = EnumDecl String EnumSpec
              | StructDecl String StructSpec
              | UnionDecl String UnionSpec
              | SimpleDecl String SimpleSpec
                deriving (Show, Typeable, Data)

newtype EnumSpec = EnumSpec [ConstDecl]
    deriving (Show, Typeable, Data)

newtype StructSpec = StructSpec [TypeDecl]
    deriving (Show, Typeable, Data)

-- | A union consists of a selector type, a set of cases and possibly
-- a default case.
data UnionSpec = UnionSpec UnionDis [(ConstExpr, UnionArm)] (Maybe UnionArm)
                 deriving (Show, Typeable, Data)

-- | The type of discriminant is either "int", "unsigned int", or an
-- enumerated type, such as "bool".
data UnionDis = UnionDis String TypeSpec
                deriving (Show, Typeable, Data)

-- | The component types are called "arms" of the union, and are preceded by
-- the value of the discriminant which implies their encoding.  Void is used
-- when an arm does not contain any data.
data UnionArm = DeclArm TypeDecl
              | VoidArm
                deriving (Show, Typeable, Data)

data SimpleSpec = PlainSpec TypeSpec
                | ArraySpec TypeSpec ConstExpr
                | VarArraySpec TypeSpec (Maybe ConstExpr)
                | OpaqueSpec ConstExpr
                | VarOpaqueSpec (Maybe ConstExpr)
                | StringSpec (Maybe ConstExpr)
                | PointerSpec TypeSpec
                  deriving (Show, Typeable, Data)

data TypeSpec = IntSpec
              | UIntSpec
              | HyperSpec
              | UHyperSpec
              | FloatSpec
              | DoubleSpec
              | QuadrupleSpec
              | BoolSpec
              | NamedSpec String
                deriving (Show, Typeable, Data)

-- | Evaluates a constant expression.
evalConstExpr :: ConstExpr -> Integer
evalConstExpr (LitExpr n) = n
evalConstExpr (NamedExpr (ConstDecl _ e)) = evalConstExpr e
evalConstExpr (BinExpr o c1 c2) = evalOp . fromJust . flip lookup ops $ o
  where
    evalOp op = op (evalConstExpr c1) (evalConstExpr c2)
    ops = [ (MUL, (*))
          , (DIV, div)
          , (MOD, mod)
          , (ADD, (+))
          , (SUB, (-))
          , (SHL, flip $ flip shiftL . fromIntegral)
          , (SHR, flip $ flip shiftR . fromIntegral)
          , (AND, (.&.))
          , (XOR, xor)
          , (OR, (.|.))
          ]
evalConstExpr (UnExpr NEG c) = negate . evalConstExpr $ c
evalConstExpr (UnExpr NOT c) = complement . evalConstExpr $ c

-- | Extract name from declaration.
declName :: TypeDecl -> String
declName (EnumDecl n _) = n
declName (StructDecl n _) = n
declName (UnionDecl n _) = n
declName (SimpleDecl n _) = n

typeDecls :: [Definition] -> [TypeDecl]
typeDecls =
    foldr f []
  where
    f (TypeDef td) tds = td : tds
    f _ tds = tds
