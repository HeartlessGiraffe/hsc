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
import Lexer.Lexer
import qualified Text.PrettyPrint as PP
import Utils (Pretty (..))

-- * SC AST

-- ** AST Definition

-- program = Program(function_definition)
-- function_definition = Function(identifier name, statement body)
-- statement = Return(exp)
-- exp = Constant(int)
--     | Unary(unary_operator, exp)
--     | Binary(binary_operator, exp, exp)
-- unary_operator = Complement | Negate
-- binary_operator = Add | Subtract | Multiply | Divide | Remainder
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

data UnaryOperator = Complement | Negate
  deriving (Show, Eq)

data BinaryOperator = Add | Subtract | Multiply | Divide | Remainder
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
    PP.text "Constant(" <> PP.int value <> PP.text ")"
  pretty (Unary uOp e) =
    PP.text "Unary(" <> pretty uOp <> PP.text ", " <> pretty e <> PP.text ")"
  pretty (Binary bOp e1 e2) =
    if isConstant e1 && isConstant e2
      then
        PP.text "Binary(" <> pretty bOp <> PP.text ", " <> pretty e1 <> PP.text ", " <> pretty e2 <> PP.text ")"
      else PP.text "Binary(" PP.$$ PP.nest 2 (pretty bOp) <> PP.text ", " PP.$$ PP.nest 2 (pretty e1) <> PP.text ", " PP.$$ PP.nest 2 (pretty e2) PP.$$ PP.text ")"

instance Pretty UnaryOperator where
  pretty Complement = PP.text "Complement"
  pretty Negate = PP.text "Negate"

instance Pretty BinaryOperator where
  pretty Add = PP.text "Add"
  pretty Subtract = PP.text "Subtract"
  pretty Multiply = PP.text "Multiply"
  pretty Divide = PP.text "Divide"
  pretty Remainder = PP.text "Remainder"

-- * Parsers

-- <program> ::= <function>
-- <function> ::= "int" <identifier> "(" "void" ")" "{" <statement> "}"
-- <statement> ::= "return" <exp> ";"
-- <exp> ::= <factor> | <exp> <binop> <exp>
-- <factor> ::= <int> | <unop> <factor> | "(" <exp> ")"
-- <unop> ::= "-" | "~"
-- <binop> ::= "-" | "+" | "*" | "/" | "%"
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
  returnVal <- parseExp minimumPrecedence
  expect TSemicolon
  return (Return returnVal)

-- | Parse a expression
--
-- <exp> ::= <factor> | <exp> <binop> <exp>
parseExp :: Precedence -> Parser Exp
parseExp p =
  let loop _left _nextToken =
        if mTIsBinary _nextToken && mTPrecedenceGEt p _nextToken
          then do
            operator <- parseBinop
            right <- parseExp (precedence (fromJust _nextToken) + 1)
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
    Just (TConstant _) -> Constant <$> parseInt
    Just TBitwiseComple -> do
      operator <- parseUnop
      Unary operator <$> parseFactor
    Just TNeg -> do
      operator <- parseUnop
      Unary operator <$> parseFactor
    Just TLeftParen -> do
      takeToken
      innerExp <- parseExp minimumPrecedence
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

-- | Parse a bineryOperator
--
-- <binop> ::= "-" | "+" | "*" | "/" | "%"
parseBinop :: Parser BinaryOperator
parseBinop = do
  nextToken <- peek
  case nextToken of
    Just TNeg -> do
      takeToken
      return Subtract
    Just TPlus -> do
      takeToken
      return Add
    Just TMul -> do
      takeToken
      return Multiply
    Just TDiv -> do
      takeToken
      return Divide
    Just TRem -> do
      takeToken
      return Remainder
    others ->
      throwExpectError [TNeg, TPlus, TMul, TDiv, TRem] others

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
