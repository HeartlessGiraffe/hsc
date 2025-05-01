{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Parser.Parser
  ( -- * AST
    Identifier (..),
    Program (..),
    FuncDef (..),
    Statement (..),
    Exp (..),
    isConstant,
    UnaryOperator (..),
    BinaryOperator (..),

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
    parseBinop,
    parseIdentifier,
    parseInt,
  )
where

import Control.Monad.Except
import Control.Monad.State
import Data.Maybe (fromJust)
import qualified Lexer.Lexer as Lexer

-- * SC AST

-- ** AST Definition

--
-- program = Program(function_definition)
-- function_definition = Function(identifier name, statement body)
-- statement = Return(exp)
-- exp = Constant(int)
--     | Unary(unary_operator, exp)
--     | Binary(binary_operator, exp, exp)
-- unary_operator = Complement | Negate | Not
-- binary_operator = Add | Subtract | Multiply | Divide | Remainder | And | Or
--                 | Equal | NotEqual | LessThan | LessOrEqual
--                 | GreaterThan | GreaterOrEqual

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

data Exp
  = Constant Int
  | Unary UnaryOperator Exp
  | Binary BinaryOperator Exp Exp
  deriving (Show, Eq)

isConstant :: Exp -> Bool
isConstant (Constant _) = True
isConstant _ = False

data UnaryOperator = Complement | Negate | Not
  deriving (Show, Eq)

data BinaryOperator
  = Add
  | Subtract
  | Multiply
  | Divide
  | Remainder
  | And
  | Or
  | Equal
  | NotEqual
  | LessThan
  | LessOrEqual
  | GreaterThan
  | GreaterOrEqual
  deriving (Show, Eq)

-- ** Pretty


-- * Parsers

-- <program> ::= <function>
-- <function> ::= "int" <identifier> "(" "void" ")" "{" <statement> "}"
-- <statement> ::= "return" <exp> ";"
-- <exp> ::= <factor> | <exp> <binop> <exp>
-- <factor> ::= <int> | <unop> <factor> | "(" <exp> ")"
-- <unop> ::= "-" | "~" | "!"
-- <binop> ::= "-" | "+" | "*" | "/" | "%" | "&&" | "||"
--         | "==" | "!=" | "<" | "<=" | ">" | ">="
-- <identifier> ::= ? An identifier token ?
-- <int> ::= ? A constant token ?

data ExpectError = ExpectError
  { expected :: Lexer.Tokens,
    found :: Maybe Lexer.Token
  }
  deriving (Show)

throwExpectError :: Lexer.Tokens -> Maybe Lexer.Token -> Parser a
throwExpectError expecting meet =
  throwError $ ExpectError expecting meet

nothingExpected :: Lexer.Tokens
nothingExpected = Lexer.emptyToken

type Expect = Either ExpectError

type Parser a = StateT Lexer.Tokens Expect a

expect :: Lexer.Token -> Parser ()
expect expectedToken = do
  nextToken <- peek
  case nextToken of
    Just foundToken -> do
      if expectedToken == foundToken
        then takeToken
        else throwExpectError (Lexer.singleToken expectedToken) (Just foundToken)
    Nothing -> throwExpectError (Lexer.singleToken expectedToken) Nothing

peek :: Parser (Maybe Lexer.Token)
peek = do
  gets (Lexer.lookupToken 0)

takeToken :: Parser ()
takeToken = do
  tokens <- get
  put $ Lexer.dropToken 1 tokens

-- | Parse a program and return the AST
evalParse :: Lexer.Tokens -> Expect Program
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
  expect Lexer.IntKeyword
  functionName <- parseIdentifier
  expect Lexer.LeftParen
  expect Lexer.VoidKeyword
  expect Lexer.RightParen
  expect Lexer.LeftBrace
  body <- parseStatement
  expect Lexer.RightBrace
  return (Function functionName body)

-- | Parse a statement
--
-- <statement> ::= "return" <exp> ";"
parseStatement :: Parser Statement
parseStatement = do
  expect Lexer.ReturnKeyword
  returnVal <- parseExp Lexer.minimumPrecedence
  expect Lexer.Semicolon
  return (Return returnVal)

-- | Parse a expression
--
-- <exp> ::= <factor> | <exp> <binop> <exp>
parseExp :: Lexer.Precedence -> Parser Exp
parseExp p =
  let loop _left _nextToken =
        if Lexer.mTIsBinary _nextToken && Lexer.mTPrecedenceGEt p _nextToken
          then do
            operator <- parseBinop
            right <- parseExp (Lexer.precedence (fromJust _nextToken) + 1)
            let nleft = Binary operator _left right
            nextT <- peek
            loop nleft nextT
          else
            return _left
   in do
        left <- parseFactor
        nextToken <- peek
        loop left nextToken

-- | Parse a factor
--
-- <factor> ::= <int> | <unop> <factor> | "(" <exp> ")"
parseFactor :: Parser Exp
parseFactor = do
  nextToken <- peek
  case nextToken of
    Just (Lexer.Constant _) -> Constant <$> parseInt
    Just Lexer.BitwiseComple -> do
      operator <- parseUnop
      Unary operator <$> parseFactor
    Just Lexer.Neg -> do
      operator <- parseUnop
      Unary operator <$> parseFactor
    Just Lexer.Not -> do
      operator <- parseUnop
      Unary operator <$> parseFactor
    Just Lexer.LeftParen -> do
      takeToken
      innerExp <- parseExp Lexer.minimumPrecedence
      expect Lexer.RightParen
      return innerExp
    others ->
      throwExpectError (Lexer.tokensFromList [Lexer.Constant 0, Lexer.BitwiseComple, Lexer.Neg, Lexer.Not, Lexer.LeftParen]) others

-- | Parse a unaryOperator
--
-- <unop> ::= "-" | "~" | "!"
parseUnop :: Parser UnaryOperator
parseUnop = do
  nextToken <- peek
  case nextToken of
    Just Lexer.BitwiseComple -> do
      takeToken
      return Complement
    Just Lexer.Neg -> do
      takeToken
      return Negate
    Just Lexer.Not -> do
      takeToken
      return Not
    others ->
      throwExpectError (Lexer.tokensFromList [Lexer.BitwiseComple, Lexer.Neg, Lexer.Not]) others

-- | Parse a bineryOperator
--
-- <binop> ::= "-" | "+" | "*" | "/" | "%" | "&&" | "||"
--         | "==" | "!=" | "<" | "<=" | ">" | ">="
parseBinop :: Parser BinaryOperator
parseBinop = do
  nextToken <- peek
  case nextToken of
    Just Lexer.Neg -> do
      takeToken
      return Subtract
    Just Lexer.Plus -> do
      takeToken
      return Add
    Just Lexer.Mul -> do
      takeToken
      return Multiply
    Just Lexer.Div -> do
      takeToken
      return Divide
    Just Lexer.Rem -> do
      takeToken
      return Remainder
    Just Lexer.And -> do
      takeToken
      return And
    Just Lexer.Or -> do
      takeToken
      return Or
    Just Lexer.TEQ -> do
      takeToken
      return Equal
    Just Lexer.TNE -> do
      takeToken
      return NotEqual
    Just Lexer.TLT -> do
      takeToken
      return LessThan
    Just Lexer.TGT -> do
      takeToken
      return GreaterThan
    Just Lexer.TLE -> do
      takeToken
      return LessOrEqual
    Just Lexer.TGE -> do
      takeToken
      return GreaterOrEqual
    others ->
      throwExpectError (Lexer.tokensFromList [Lexer.Neg, Lexer.Plus, Lexer.Mul, Lexer.Div, Lexer.Rem, Lexer.And, Lexer.Or, Lexer.TEQ, Lexer.TNE, Lexer.TLT, Lexer.TGT, Lexer.TLE, Lexer.TGE]) others

-- | Parse an identifier
--
-- <identifier> ::= ? An identifier token ?
parseIdentifier :: Parser Identifier
parseIdentifier = do
  nextToken <- peek
  case nextToken of
    Just (Lexer.Identifier i) -> do
      takeToken
      return (Identifier i)
    others ->
      throwExpectError (Lexer.tokensFromList [Lexer.Identifier ""]) others

-- | Parse an integer
--
-- <int> ::= ? An integer token ?
parseInt :: Parser Int
parseInt = do
  nextToken <- peek
  case nextToken of
    Just (Lexer.Constant i) -> do
      takeToken
      return i
    others ->
      throwExpectError (Lexer.tokensFromList [Lexer.Constant 0]) others
