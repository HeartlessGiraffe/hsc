{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Parser.Parser
  ( -- * AST
    Identifier (..),
    Program (..),
    FuncDef (..),
    Statement (..),
    Exp (..),
    UnaryOperator (..), 

    -- * Parsers
    Expect,
    ExpectError (..),
    Parser,
    evalParse,
    parseProgram,
    parseFuncDef,
    parseStatement,
    parseExp,
    parseUnop,
    parseIdentifier,
    parseInt,
  )
where

import Control.Monad.Except
import Control.Monad.State
import Lexer.Lexer
import qualified Text.PrettyPrint as PP
import Utils (Pretty (..))

-- * SC AST

-- ** AST Definition

-- program = Program(function_definition)
-- function_definition = Function(identifier name, statement body)
-- statement = Return(exp)
-- exp = Constant(int) | Unary(unary_operator, exp)
-- unary_operator = Complement | Negate

newtype Identifier = Identifier
  { unIdenityfier :: String
  }
  deriving (Show, Eq)

data Program = Program FuncDef 
  deriving (Show, Eq)

data FuncDef = Function
  { _funcName :: Identifier,
    _funcBody :: Statement
  }
  deriving (Show, Eq)

data Statement = Return Exp 
  deriving (Show, Eq)

data Exp = Constant Int | Unary UnaryOperator Exp 
  deriving (Show, Eq)

data UnaryOperator = Complement | Negate 
  deriving (Show, Eq)

-- ** Pretty

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
  pretty (Unary uOp e) = 
    PP.text "Unary (" <> pretty uOp <> PP.text " (" <> pretty e <> PP.text ")" <> PP.text ")"

instance Pretty UnaryOperator where 
  pretty Complement = PP.text "Complement"
  pretty Negate = PP.text "Negate"

-- * Parsers

-- <program> ::= <function>
-- <function> ::= "int" <identifier> "(" "void" ")" "{" <statement> "}"
-- <statement> ::= "return" <exp> ";"
-- <exp> ::= <int> | <unop> <exp> | "(" <exp> ")"
-- <unop> ::= "-" | "~"
-- <identifier> ::= ? An identifier token ?
-- <int> ::= ? A constant token ?

data ExpectError = ExpectError
  { expected :: [Token],
    found :: Maybe Token
  }
  deriving (Show)

throwExpectError :: [Token] -> Maybe Token -> Parser a
throwExpectError expecting meet = 
  throwError $ ExpectError expecting meet

nothingExpected :: [Token]
nothingExpected = []

type Expect = Either ExpectError

type Parser a = StateT Tokens Expect a

expect :: Token -> Parser ()
expect expectedToken = do
  nextToken <- peek
  case nextToken of
    Just foundToken -> do
      if expectedToken == foundToken
        then takeToken
        else throwExpectError [expectedToken] (Just foundToken)
    Nothing -> throwExpectError [expectedToken] Nothing

peek :: Parser (Maybe Token)
peek = do 
  tokens <- get 
  case tokens of 
    (foundToken : _) -> return (Just foundToken)
    [] -> return Nothing

takeToken :: Parser ()
takeToken = do 
  tokens <- get 
  put $ drop 1 tokens

-- | Parse a program and return the AST
evalParse :: Tokens -> Expect Program
evalParse = evalStateT parseProgram

-- | Parse a program
--
-- <program> ::= <function>
parseProgram :: Parser Program
parseProgram = do
  program <- parseFuncDef
  nextToken <- peek
  case nextToken of
    Nothing -> return (Program program)
    others -> throwExpectError nothingExpected others
    
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
-- <exp> ::= <int> | <unop> <exp> | "(" <exp> ")
parseExp :: Parser Exp
parseExp = do
  nextToken <- peek 
  case nextToken of 
    Just (TConstant _) -> Constant <$> parseInt
    Just TBitwiseComple -> do 
      operator <- parseUnop 
      Unary operator <$> parseExp
    Just TNeg -> do 
      operator <- parseUnop 
      Unary operator <$> parseExp
    Just TLeftParen -> do 
      takeToken
      innerExp <- parseExp 
      expect TRightParen
      return innerExp
    others -> 
      throwExpectError [TConstant 0, TBitwiseComple, TNeg, TLeftParen] others


-- | Parse a unaryOperator
--
-- <unop> ::= "-" | "~"
parseUnop :: Parser UnaryOperator
parseUnop = do 
  nextToken <- peek 
  case nextToken of 
    Just TBitwiseComple -> do 
      takeToken
      return Complement
    Just TNeg -> do 
      takeToken
      return Negate
    others -> 
      throwExpectError [TBitwiseComple, TNeg] others

-- | Parse an identifier
--
-- <identifier> ::= ? An identifier token ?
parseIdentifier :: Parser Identifier
parseIdentifier = do
  nextToken <- peek
  case nextToken of
    Just (TIdentifier i) -> do
      takeToken
      return (Identifier i)
    others -> 
      throwExpectError [TIdentifier ""] others

-- | Parse an integer
--
-- <int> ::= ? An integer token ?
parseInt :: Parser Int
parseInt = do
  nextToken <- peek
  case nextToken of
    Just (TConstant i) -> do
      takeToken
      return i
    others -> 
      throwExpectError [TConstant 0] others
      
