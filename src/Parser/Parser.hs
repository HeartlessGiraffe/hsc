{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Parser.Parser
  ( -- * AST
    Identifier (..),
    Program (..),
    FuncDef (..),
    Statement (..),
    Exp (..),

    -- * Parsers
    Parser,
    Expect,
    ExpectError (..),
    evalParse,
    parseProgram,
    parseFuncDef,
    parseStatement,
    parseExp,
    parseIdentifier,
    parseInt,
  )
where

import Control.Monad.Except
import Control.Monad.State
import Lexer.Lexer
import qualified Text.PrettyPrint as PP
import Utils (Pretty (..))

-- * AST

newtype Identifier = Identifier
  { unIdenityfier :: String
  }
  deriving (Show, Eq)

data Program = Program FuncDef deriving (Show, Eq)

data FuncDef = Function
  { _funcName :: Identifier,
    _funcBody :: Statement
  }
  deriving (Show, Eq)

data Statement = Return Exp deriving (Show, Eq)

data Exp = Constant Int deriving (Show, Eq)

-- * Pretty Printing

instance Pretty Identifier where
  pretty (Identifier name) = PP.text name

instance Pretty Program where
  pretty (Program funcDef) =
    PP.text "Program {" PP.$$ PP.nest 2 (pretty funcDef) PP.$$ PP.text "}"

instance Pretty FuncDef where
  pretty (Function name body) =
    PP.text "Function (" <> pretty name <> PP.text ") {" PP.$$ PP.nest 2 (pretty body) PP.$$ PP.text "}"

instance Pretty Statement where
  pretty (Return expr) =
    PP.text "Return (" <> pretty expr <> PP.text ")"

instance Pretty Exp where
  pretty (Constant value) =
    PP.text "Constant (" <> PP.int value <> PP.text ")"

-- * Parsers

data ExpectError = ExpectError
  { expected :: Maybe Token,
    found :: Maybe Token
  }
  deriving (Show)

type Expect = Either ExpectError

type Parser a = StateT Tokens Expect a

expect :: Token -> Parser ()
expect expectedToken = do
  tokens <- get
  case tokens of
    (foundToken : rest) -> do
      if expectedToken == foundToken
        then put rest
        else throwError $ ExpectError (Just expectedToken) (Just foundToken)
    [] -> throwError $ ExpectError (Just expectedToken) Nothing

-- | Parse a program and return the AST
evalParse :: Tokens -> Expect Program
evalParse = evalStateT parseProgram

-- | Parse a program
--
-- <program> ::= <function>
parseProgram :: Parser Program
parseProgram = do
  program <- parseFuncDef
  rest <- get
  case rest of
    token : _ -> throwError $ ExpectError Nothing (Just token)
    _ -> return (Program program)

-- | Parse a function definition
--
-- <function> ::= "int" <identifier> "(" "void" ")" "{" <statement> "}"
parseFuncDef :: Parser FuncDef
parseFuncDef = do
  expect TIntKeyword
  functionName <- parseIdentifier
  expect TLeftParen
  expect TVoidKeyword
  expect TRightParen
  expect TLeftBrace
  body <- parseStatement
  expect TRightBrace
  return (Function functionName body)

-- | Parse a statement
--
-- <statement> ::= "return" <exp> ";"
parseStatement :: Parser Statement
parseStatement = do
  expect TReturnKeyword
  returnVal <- parseExp
  expect TSemicolon
  return (Return returnVal)

-- | Parse a expression
--
-- <exp> ::= <int>
parseExp :: Parser Exp
parseExp = do
  Constant <$> parseInt

-- | Parse an identifier
--
-- <identifier> ::= ? An identifier token ?
parseIdentifier :: Parser Identifier
parseIdentifier = do
  tokens <- get
  case tokens of
    (TIdentifier i : rest) -> do
      put rest
      return (Identifier i)
    (other : _) -> do
      throwError $ ExpectError (Just (TIdentifier "")) (Just other)
    [] -> do
      throwError $ ExpectError (Just (TIdentifier "")) Nothing

-- | Parse an integer
--
-- <int> ::= ? An integer token ?
parseInt :: Parser Int
parseInt = do
  tokens <- get
  case tokens of
    (TConstant i : rest) -> do
      put rest
      return i
    (other : _) -> do
      throwError $ ExpectError (Just (TConstant 0)) (Just other)
    [] -> do
      throwError $ ExpectError (Just (TConstant 0)) Nothing
