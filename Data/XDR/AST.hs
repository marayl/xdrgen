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
    , promoteAnons
    ) where

import Control.Monad.State
import Data.Bits
import Data.Generics
import Data.List
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
                 deriving (Eq, Show, Typeable, Data)

data BinOp = MUL | DIV | MOD | ADD | SUB | SHL | SHR | AND | XOR | OR
             deriving (Eq, Show, Typeable, Data)

data UnOp = NEG | NOT
            deriving (Eq, Show, Typeable, Data)

data ConstDecl = ConstDecl String ConstExpr
                 deriving (Eq, Show, Typeable, Data)

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
                  deriving (Eq, Show, Typeable, Data)

data TypeDecl = EnumDecl String EnumSpec
              | StructDecl String StructSpec
              | UnionDecl String UnionSpec
              | SimpleDecl String SimpleSpec
                deriving (Eq, Show, Typeable, Data)

newtype EnumSpec = EnumSpec [ConstDecl]
    deriving (Eq, Show, Typeable, Data)

newtype StructSpec = StructSpec [TypeDecl]
    deriving (Eq, Show, Typeable, Data)

-- | A union consists of a selector type, a set of cases and possibly
-- a default case.
data UnionSpec = UnionSpec UnionDis [(ConstExpr, UnionArm)] (Maybe UnionArm)
                 deriving (Eq, Show, Typeable, Data)

-- | The type of discriminant is either "int", "unsigned int", or an
-- enumerated type, such as "bool".
data UnionDis = UnionDis String TypeSpec
                deriving (Eq, Show, Typeable, Data)

-- | The component types are called "arms" of the union, and are preceded by
-- the value of the discriminant which implies their encoding.  Void is used
-- when an arm does not contain any data.
data UnionArm = DeclArm TypeDecl
              | VoidArm
                deriving (Eq, Show, Typeable, Data)

data SimpleSpec = PlainSpec TypeSpec
                | ArraySpec TypeSpec ConstExpr
                | VarArraySpec TypeSpec (Maybe ConstExpr)
                | OpaqueSpec ConstExpr
                | VarOpaqueSpec (Maybe ConstExpr)
                | StringSpec (Maybe ConstExpr)
                | PointerSpec TypeSpec
                  deriving (Eq, Show, Typeable, Data)

data TypeSpec = IntSpec
              | UIntSpec
              | HyperSpec
              | UHyperSpec
              | FloatSpec
              | DoubleSpec
              | QuadrupleSpec
              | BoolSpec
              | NamedSpec String
                deriving (Eq, Show, Typeable, Data)

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

-- | State holding anonymous names for consumption, and resulting anonymous
-- types for promotion.

type AnonState a = State ([String], [TypeDecl]) a

-- | Given a struct or union element name, and an anonymous type constructor,
-- add anonymous type to state and return a typedef to it.

promoteAnon :: String -> (String -> TypeDecl) -> AnonState TypeDecl
promoteAnon n f = do
    (x:xs, ds) <- get
    put (xs, (f x):ds)
    return (SimpleDecl n (PlainSpec (NamedSpec x)))

promoteEnum :: String -> EnumSpec -> AnonState TypeDecl
promoteEnum n =
    promoteAnon n . flip EnumDecl

promoteStruct :: String -> StructSpec -> AnonState TypeDecl
promoteStruct n =
    promoteAnon n . flip StructDecl

promoteUnion :: String -> UnionSpec -> AnonState TypeDecl
promoteUnion n =
    promoteAnon n . flip UnionDecl

-- | Flush all anonymous type declarations from state monad.

flushAnons :: AnonState [TypeDecl]
flushAnons = do
    (xs, ds) <- get
    put (xs, [])
    return ds

-- | Promote all anonymous types to top-level definitions.

promoteAnons :: ModuleSpec -> ModuleSpec
promoteAnons (ModuleSpec n is ds) =
    ModuleSpec n is ds'
  where
    ds' = concat . evalState (mapM promoteDef ds)
          $ (map ((anonPrefix n ++) . show) [1..], [])

-- | Descend into typedef and promote all anonymous types into top-level
-- definitions.

promoteDef :: Definition -> AnonState [Definition]
promoteDef (TypeDef td) = do
    d <- topDecl td
    ds <- flushAnons
    return . map TypeDef . reverse $ (d : ds)
promoteDef d@(ConstDef _) = do
    return [d]

-- | The prefix assigned to top-level anonymous types.

anonPrefix :: ModuleName -> String
anonPrefix (ModuleName xs) =
    (concat . intersperse "_" $ xs) ++ "_anon"

-- | Descend into top-level type declarations.

topDecl :: TypeDecl -> AnonState TypeDecl
topDecl d@(EnumDecl _ _) = do
    return d
topDecl (StructDecl n (StructSpec ds)) = do
    ds' <- mapM memDecl ds
    return (StructDecl n (StructSpec ds'))
topDecl (UnionDecl n (UnionSpec ud cs md)) = do
    cs' <- mapM unionCase cs
    md' <- unionDflt md
    return (UnionDecl n (UnionSpec ud cs' md'))
topDecl d@(SimpleDecl _ _) = do
    return d

-- | Put each member with an anonymous type into the state monad, and replace
-- anonymous type with a typedef to the new top-level type.

memDecl :: TypeDecl -> AnonState TypeDecl
memDecl (EnumDecl n s) =
    promoteEnum n s
memDecl (StructDecl n (StructSpec ds)) = do
    ds' <- mapM memDecl ds
    promoteStruct n (StructSpec ds')
memDecl (UnionDecl n (UnionSpec ud cs md)) = do
    cs' <- mapM unionCase cs
    md' <- unionDflt md
    promoteUnion n (UnionSpec ud cs' md')
memDecl d@(SimpleDecl _ _) = do
    return d

unionCase :: (ConstExpr, UnionArm) -> AnonState (ConstExpr, UnionArm)
unionCase (c, d) =
    ((,) c) `fmap` unionArm d

unionDflt :: Maybe UnionArm -> AnonState (Maybe UnionArm)
unionDflt =
    maybe (return Nothing) (fmap Just . unionArm)

unionArm :: UnionArm -> AnonState UnionArm
unionArm (DeclArm d) = do
    d' <- memDecl d
    return . DeclArm $ d'
unionArm VoidArm = do
    return VoidArm
