module Data.XDR.Parser
    ( LanguageOptions (..)
    , parseConstExpr
    , parseString
    , parseFile
    , ParseError
    ) where

import Control.Applicative hiding (many, (<|>))
import Control.Arrow
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Foldable hiding (concat)
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import System.Path

import Text.Parsec hiding (ParseError)
import Text.Parsec.ByteString hiding (Parser)
import Text.Parsec.Token (makeTokenParser, GenLanguageDef (..))
import qualified Text.Parsec.Token as T
import Text.Parsec.Expr

import Data.XDR.AST
import Data.XDR.PathUtils

-- | Lexer
l :: (Monad m) => T.GenTokenParser ByteString Context m
l = makeTokenParser
    LanguageDef { commentStart = "/*"
                , commentEnd = "*/"
                , commentLine = "//"
                , nestedComments = True
                , identStart = letter
                , identLetter = alphaNum <|> char '_'
                , opStart = oneOf "+-/*=."
                , opLetter = oneOf "+-/*=."
                , reservedNames = [ "FALSE"
                                  , "TRUE"
                                  , "bool"
                                  , "case"
                                  , "const"
                                  , "default"
                                  , "double"
                                  , "enum"
                                  , "float"
                                  , "hyper"
                                  , "import"
                                  , "int"
                                  , "module"
                                  , "opaque"
                                  , "quadruple"
                                  , "string"
                                  , "struct"
                                  , "switch"
                                  , "typedef"
                                  , "union"
                                  , "unsigned"
                                  , "void"
                                  ]
                , reservedOpNames = [ "+"
                                    , "-"
                                    , "~"
                                    , "*"
                                    , "/"
                                    , "%"
                                    , "<<"
                                    , ">>"
                                    , "&"
                                    , "^"
                                    , "|"
                                    , "."
                                    ]
                , caseSensitive = True
                }
    where
      tokens = [ "bool"
               , "case"
               , "const"
               , "default"
               , "double"
               , "enum"
               , "float"
               , "hyper"
               , "import"
               , "int"
               , "module"
               , "opaque"
               , "quadruple"
               , "string"
               , "struct"
               , "switch"
               , "typedef"
               , "union"
               , "unsigned"
               , "void"
               ]

angles, braces, parens, squares :: (Monad m) => Parser m a -> Parser m a
angles = T.angles l
braces = T.braces l
parens = T.parens l
squares = T.squares l

colon, comma, semi :: (Monad m) => Parser m String
colon = T.colon l
comma = T.comma l
semi = T.semi l

commaSep, commaSep1 :: (Monad m) => Parser m a -> Parser m [a]
commaSep = T.commaSep l
commaSep1 = T.commaSep1 l

identifier :: (Monad m) => Parser m String
identifier = T.identifier l

-- | Use natural instead of integer, because signedness is already handled by
-- const expresssions.
natural :: (Monad m) => Parser m Integer
natural = T.natural l

reserved, reservedOp :: (Monad m) => String -> Parser m ()
reserved = T.reserved l
reservedOp = T.reservedOp l

stringLiteral :: (Monad m) => Parser m String
stringLiteral = T.stringLiteral l

whiteSpace :: (Monad m) => Parser m ()
whiteSpace = T.whiteSpace l

-- | Extensions to the basic XDR language can be turned on with these options.
data LanguageOptions =
  -- | Allows the caller to predefine some constant values.  e.g. True == 1
    Defines { constantDefinitions :: [(String, Integer)] }

  -- | Turns on the imports extension.  Imported files are searched
  --   for in the directories given.
  | Imports { importDirs :: [AbsDir] }
                     deriving (Show, Eq)

-- | Trivial error type
newtype ParseError = ParseError String

instance Show ParseError where
    show (ParseError str) = str

parseConstExpr :: ByteString -> String -> Either [ParseError] ConstExpr
parseConstExpr txt source =
  case runParser constExpr (initContext []) source txt of
    Left err -> Left [ParseError . show $ err]
    Right spec -> Right spec

-- | Parse a string.  The Imports language extension is not available
--   via this parser since it doesn't run in the IO Monad (fix this).
parseString :: [LanguageOptions] -> ByteString -> String -> Either [ParseError] ModuleSpec
parseString options txt source =
  case runParser moduleSpec (initContext defines) source txt of
    Left err -> Left [ParseError . show $ err]
    Right spec -> Right spec
  where
    defines = concat [cs | (Defines cs) <- options]

-- | Parse a file.  The Imports language extension is available.
parseFile :: [LanguageOptions] -> AbsFile -> IO (Either [ParseError] ModuleSpec)
parseFile options path =
  parseImportSpecification defines includes path
  where
    defines = concat [cs | (Defines cs) <- options]
    includes = concat [cs | (Imports cs) <- options]

{-  do
  input <- B.readFile path'
  return $ parseString options input path'
    where
      path' = getPathString' path
-}

data ImportSpec = ImportSpec [RelFile] [Definition]

-- FIXME: using ExceptionT IO would simplify this
parseImportSpecification :: [(String, Integer)] -> [AbsDir] -> AbsFile -> IO (Either [ParseError] ModuleSpec)
parseImportSpecification defines includes path = do
  txt <- B.readFile path'
  result <-  runParserT (importSpec includes) (initContext defines) path' txt
  case result of
    Left errs -> return $ Left [ParseError . show $ errs]
    Right ispec -> return $ Right ispec
  where
    path' = getPathString' path

data Context = Context { constTable :: Map String ConstExpr
                       , nextEnum :: Integer
                       }

initContext :: [(String, Integer)] -> Context
initContext defines = Context (M.fromList . map (second LitExpr) $ defines) 0

-- type Parser = GenParser Char Context
type Parser = ParsecT ByteString Context

-- | Primary expression.
primary :: (Monad m) => Parser m ConstExpr
primary = (LitExpr <$> natural)
          <|> try (LitExpr <$> boolean)
          <|> (identifier >>= findReference)

-- | Interpret TRUE and FALSE as const-literals.
boolean :: (Monad m) => Parser m Integer
boolean = (1 <$ string "TRUE") <|> (0 <$ string "FALSE")

findReference :: (Monad m) => String -> Parser m ConstExpr
findReference n = do
  c <- getState
  case M.lookup n (constTable c) of
    Nothing -> unexpected $ "unknown constant '" ++ n ++ "'"
    Just e -> pure . NamedExpr . ConstDecl n $ e

-- Operator Precedence:
-- Unary: + - ~
-- Multiplicative: * / %
-- Additive: + -
-- Shift: << >>
-- Bitwise AND: &
-- Bitwise XOR: ^
-- Bitwise OR: |

constExpr :: (Monad m) => Parser m ConstExpr
constExpr =
    buildExpressionParser table term
  where
    table = [ [ prefix "+" id
              , prefix "-" (UnExpr NEG)
              , prefix "~" (UnExpr NOT)
              ]
            , [ binary "*" (BinExpr MUL) AssocLeft
              , binary "/" (BinExpr DIV) AssocLeft
              , binary "%" (BinExpr MOD) AssocLeft
              ]
            , [ binary "+" (BinExpr ADD) AssocLeft
              , binary "-" (BinExpr SUB) AssocLeft
              ]
            , [ binary "<<" (BinExpr SHL) AssocLeft
              , binary ">>" (BinExpr SHR) AssocLeft
              ]
            , [ binary "&" (BinExpr AND) AssocLeft
              ]
            , [ binary "^" (BinExpr XOR) AssocLeft
              ]
            , [ binary "|" (BinExpr OR) AssocLeft
              ]
            ]
    prefix name fun = Prefix (mkOp name fun)
    binary name fun = Infix (mkOp name fun)
    mkOp name fun = reservedOp name *> pure fun
    term = primary <|> parens constExpr

definition :: (Monad m) => Parser m Definition
definition =
    choice [ ConstDef <$> constDecl <* semi
           , TypeDef <$> topDecl <* semi
           ] <?> "definition"

constDecl :: (Monad m) => Parser m ConstDecl
constDecl =
    ((,) <$> (reserved "const" *> identifier <* reservedOp "=")
             <*> constExpr)
    >>= uncurry insertConstDecl

-- | Without tag namespaces, top-level enum, struct and union declarations are
-- simply an abbreviated form of the alternative typedef syntax.
topDecl :: (Monad m) => Parser m TypeDecl
topDecl =
    choice [ EnumDecl <$> (reserved "enum" *> identifier) <*> enumSpec
           , StructDecl <$> (reserved "struct" *> identifier) <*> structSpec
           , UnionDecl <$> (reserved "union" *> identifier) <*> unionSpec
           , reserved "typedef" *> typeDecl
           ]

-- | Enum body.
enumSpec :: (Monad m) => Parser m EnumSpec
enumSpec =
    EnumSpec <$> (setNextEnum 0 *> braces body)
  where
    body = commaSep pair
    pair = do
      n <- identifier
      mc <- optionMaybe (reservedOp "=" *> constExpr)
      mkElem n mc

    setNextEnum n = do
      ctxt <- getState
      setState $ ctxt { nextEnum = n }

    incNextEnum = do
      ctxt <- getState
      let v = nextEnum ctxt
      setState $ ctxt { nextEnum = succ v }
      return v

    mkElem n Nothing =
        incNextEnum >>= insertConstDecl n . LitExpr
    mkElem n (Just e) =
        let v = evalConstExpr e in (setNextEnum (v + 1))
                *> insertConstDecl n e

-- | Struct body.
structSpec :: (Monad m) => Parser m StructSpec
structSpec =
    StructSpec <$> braces body
  where
    body = many1 (typeDecl <* semi)

infixl 4 <*-*>
(<*-*>) :: (Applicative f) => f (a -> b -> c) -> f (a, b) -> f c
(<*-*>) fn fab = uncurry <$> fn <*> fab

-- | Union body.
unionSpec :: (Monad m) => Parser m UnionSpec
unionSpec =
    UnionSpec <$> switchDis <*-*> braces ((,) <$> caseArms <*> dfltArm)
  where
    switchDis = reserved "switch" *> parens unionDis
    caseArms = many1 caseArm
    caseArm =
        (,) <$> (reserved "case" *> constExpr <* colon) <*> unionArm <* semi
    dfltArm = optionMaybe (reserved "default" *> colon *> unionArm <* semi)

-- | Discriminator declaration.
unionDis :: (Monad m) => Parser m UnionDis
unionDis = flip UnionDis <$> typeSpec <*> identifier

-- | Case-arm declaration.
unionArm :: (Monad m) => Parser m UnionArm
unionArm =
    choice [ DeclArm <$> typeDecl
           , reserved "void" *> pure VoidArm
           ] <?> "union-arm"

-- | Typedef body.
typeDecl :: (Monad m) => Parser m TypeDecl
typeDecl =
    choice [ flip EnumDecl <$> (reserved "enum" *> enumSpec)
                      <*> identifier
           , flip StructDecl <$> (reserved "struct" *> structSpec)
                      <*> identifier
           , flip UnionDecl <$> (reserved "union" *> unionSpec)
                      <*> identifier
           , simpleDecl
           ] <?> "type-decl"

simpleDecl :: (Monad m) => Parser m TypeDecl
simpleDecl =
    choice [ SimpleDecl <$> (reserved "string" *> identifier) <*> stringSpec
           , SimpleDecl <$> (reserved "opaque" *> identifier) <*> opaqueSpec
           , typeSpec >>= withType
           ]
  where
    withType :: (Monad m) => TypeSpec -> Parser m TypeDecl
    withType t =
        choice [ SimpleDecl <$> (reservedOp "*" *> identifier)
                                <*> pure (PointerSpec t)
               , SimpleDecl <$> identifier <*> arrayOrPlainSpec t
               ]

stringSpec :: (Monad m) => Parser m SimpleSpec
stringSpec =
    StringSpec <$> angles (optionMaybe constExpr)

opaqueSpec :: (Monad m) => Parser m SimpleSpec
opaqueSpec =
    choice [ OpaqueSpec <$> squares constExpr
           , VarOpaqueSpec <$> angles (optionMaybe constExpr)
           ]

arrayOrPlainSpec :: (Monad m) => TypeSpec -> Parser m SimpleSpec
arrayOrPlainSpec t =
    choice [ ArraySpec t <$> squares constExpr
           , VarArraySpec t <$> angles (optionMaybe constExpr)
           , pure (PlainSpec t)
           ]

typeSpec :: (Monad m) => Parser m TypeSpec
typeSpec =
    choice [ reserved "unsigned" *>
             (mkTypeSpec "int" UIntSpec
              <|> mkTypeSpec "hyper" UHyperSpec)
           , mkTypeSpec "int" IntSpec
           , mkTypeSpec "hyper" HyperSpec
           , mkTypeSpec "float" FloatSpec
           , mkTypeSpec "double" DoubleSpec
           , mkTypeSpec "quadruple" QuadrupleSpec
           , mkTypeSpec "bool" BoolSpec
           , NamedSpec <$> identifier
           ] <?> "type"

mkTypeSpec :: (Monad m) => String -> TypeSpec -> Parser m TypeSpec
mkTypeSpec keyword t = const t <$> reserved keyword

insertConstDecl :: (Monad m) => String -> ConstExpr -> Parser m ConstDecl
insertConstDecl name expr = do
    ctx <- getState
    let table = constTable ctx

    -- maybe the constant has already been defined ?  We only
    -- complain if they're trying to give it a different value.
    case M.lookup name table of
      Nothing -> insert name expr table ctx
      Just _ -> unexpected . concat $ [ "multiple definitions for "
                                      , name
                                      ]
    pure . ConstDecl name $ expr
  where
    insert name v table ctx =
        setState $ ctx { constTable = M.insert name v table }

withInput :: ByteString -> Parser IO a -> Parser IO a
withInput new p = do
  old <- getInput
  oldCtxt <- getState
  setInput new
  r <- p
  setInput old
  putState oldCtxt
  return r

moduleStmt' :: (Monad m) => Parser m ModuleName
moduleStmt' = ModuleName <$> sepBy1 segment (reservedOp ".")
    where
      segment = (:) <$> letter <*> many (alphaNum <|> char '_')

moduleStmt :: (Monad m) => Parser m ModuleName
moduleStmt = reserved "module" *> moduleStmt' <* semi

moduleToRelFile :: ModuleName -> RelFile
moduleToRelFile (ModuleName elements) =
    ((Data.List.foldl (</>) (asRelDir "") . map asRelDir $ prefix) </>
     (asRelFile file)) <.> "xdr"
    where
      prefix = reverse . tail . reverse $ elements
      file = head . reverse $ elements

importStmt :: [AbsDir] -> Parser IO ((ModuleName, ModuleSpec), Context)
importStmt includes = do
  mod <- reserved "import" *> moduleStmt' <* semi
  let path = moduleToRelFile mod
  mpath <- liftIO $ pathLookup includes path
  case mpath of
    Nothing -> unexpected $ "couldn't find import '" ++ getPathString' path ++ "'"
    Just path' -> do
      let pathTxt = getPathString' path'
      txt <- liftIO $ B.readFile pathTxt
      withInput txt ((\s c -> ((mod, s), c)) <$> importSpec includes <*> getState)

-- An xdr file that contains imports
importSpec :: [AbsDir] -> Parser IO ModuleSpec
importSpec includes = do
  ctxt <- getState
  mod <- whiteSpace *> moduleStmt
  specs <- whiteSpace *> many (importStmt includes)

  -- we need to combine all the contexts generated by the imports.
  ctxt' <- foldrM combineImports ctxt . map snd $ specs

  -- parse the rest of the file in this new uber context
  putState ctxt'
  ModuleSpec _ _ defs <- moduleSpec
  return $ ModuleSpec mod (M.fromList . map fst $ specs) defs

combineImports :: Context -> Context -> Parser IO Context
combineImports (Context m1 _) (Context m2 _) = return $ Context (M.union m1 m2) 0

-- FIXME: any defines passed in from the command line will appear as duplicates, so the following code needs updating
  -- if not . null $ duplicates
  -- then fail $ "duplicate constant definitions: " ++ show duplicates
  -- else return $ Context (M.union m1 m2) 0
  -- where
  --   duplicates = M.toList $ M.intersection m1 m2

moduleSpec :: (Monad m) => Parser m ModuleSpec
moduleSpec =
    ModuleSpec (ModuleName ["global"]) M.empty
        <$> (whiteSpace *> many definition <* eof)

----------------------------------------------------------------
