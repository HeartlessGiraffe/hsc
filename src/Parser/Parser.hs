{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Parser.Parser
  ( -- * AST
    Identifier (..),
    Program (..),
    BlockItem (..),
    Declaration (..),
    FuncDef (..),
    Statement (..),
    Exp (..),
    isConstant,
    notVar,
    UnaryOperator (..),
    BinaryOperator (..),

    -- * Parsers
    Expect,
    ParseError (..),
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
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Lexer.Lexer as Lexer
import qualified Data.Sequence as Seq
-- * SC AST

-- ** AST Definition

--
-- program = Program(function_definition)
-- function_definition = Function(identifier name, block_item* body)
-- block_item = S(statement) | D(declaration)
-- declaration = Declaration(identifier name, exp? init)
-- statement = Return(exp) | Expression(exp) | Null
-- exp = Constant(int)
--     | Var(identifier)
--     | Unary(unary_operator, exp)
--     | Binary(binary_operator, exp, exp)
--     | Assignment(exp, exp)
-- unary_operator = Complement | Negate | Not
-- binary_operator = Add | Subtract | Multiply | Divide | Remainder | And | Or
--                 | Equal | NotEqual | LessThan | LessOrEqual
--                 | GreaterThan | GreaterOrEqual

newtype Identifier = Identifier
  { unIdenityfier :: String
  }
  deriving (Show, Eq, Ord)

data Program = Program FuncDef
  deriving (Show, Eq)

data FuncDef = Function
  { _funcName :: Identifier,
    _funcBody :: BlockItems
  }
  deriving (Show, Eq)

data BlockItem = S Statement | D Declaration
  deriving (Show, Eq)

type BlockItems = Seq.Seq BlockItem

blockItemsAppend :: BlockItems -> BlockItem -> BlockItems
blockItemsAppend = (Seq.|>)

emptyBlockItem :: BlockItems
emptyBlockItem = Seq.empty

data Declaration = Declaration
  { _name :: Identifier,
    _init :: Maybe Exp
  }
  deriving (Show, Eq)

data Statement
  = Return Exp
  | Expression Exp
  | Null
  deriving (Show, Eq)

data Exp
  = Constant Int
  | Var Identifier
  | Unary UnaryOperator Exp
  | Binary BinaryOperator Exp Exp
  | Assignment Exp Exp
  deriving (Show, Eq)

isConstant :: Exp -> Bool
isConstant (Constant _) = True
isConstant _ = False

notVar :: Exp -> Bool
notVar (Var _) = False
notVar _ = True

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
-- <function> ::= "int" <identifier> "(" "void" ")" "{" { <block-item> } "}"
-- <block-item> ::= <statement> | <declaration>
-- <declaration> ::= "int" <identifier> [ "=" <exp> ] ";"
-- <statement> ::= "return" <exp> ";" | <exp> ";" | ";"
-- <exp> ::= <factor> | <exp> <binop> <exp>
-- <factor> ::= <int> | <identifier> | <unop> <factor> | "(" <exp> ")"
-- <unop> ::= "-" | "~" | "!"
-- <binop> ::= "-" | "+" | "*" | "/" | "%" | "&&" | "||"
--           | "==" | "!=" | "<" | "<=" | ">" | ">=" | "="
-- <identifier> ::= ? An identifier token ?
-- <int> ::= ? A constant token ?

data ParseError = ParseError
  { message :: String,
    expected :: Expecting,
    found :: Maybe Lexer.Token
  }
  deriving (Show)

data Expecting = Something | Tokens Lexer.Tokens
  deriving (Show)

throwExpectTokensError :: String -> Lexer.Tokens -> Maybe Lexer.Token -> Parser a
throwExpectTokensError msg expecting meet =
  throwError $ ParseError msg (Tokens expecting) meet

throwExpectSomethingError :: String -> Parser a
throwExpectSomethingError msg =
  throwError $ ParseError msg Something Nothing

nothingExpected :: Lexer.Tokens
nothingExpected = Lexer.emptyToken

type Expect = Either ParseError

type Parser a = StateT Lexer.Tokens Expect a

expect :: Lexer.Token -> Parser ()
expect expectedToken = do
  nextToken <- peek
  case nextToken of
    Just foundToken -> do
      if expectedToken == foundToken
        then takeToken
        else throwExpectTokensError "expect" (Lexer.singleToken expectedToken) (Just foundToken)
    Nothing -> throwExpectTokensError "expect" (Lexer.singleToken expectedToken) Nothing

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
    others -> throwExpectTokensError "parseProgram" nothingExpected others

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
  body <- loop emptyBlockItem
  expect Lexer.RightBrace
  return (Function functionName body)
  where
    loop :: BlockItems -> Parser BlockItems
    loop functionBody = do
      nextToken <- peek
      case nextToken of
        Just Lexer.RightBrace -> return functionBody
        _ -> do
          nextBlockItem <- parseBlockItem
          loop (blockItemsAppend functionBody nextBlockItem)

parseBlockItem :: Parser BlockItem
parseBlockItem = do
  nextToken <- peek
  case nextToken of
    Just Lexer.IntKeyword -> D <$> parseDeclaration
    _ -> S <$> parseStatement

parseDeclaration :: Parser Declaration
parseDeclaration = do
  expect Lexer.IntKeyword
  lvalue <- parseIdentifier
  nextToken <- peek
  case nextToken of
    Just Lexer.Assign -> do
      takeToken
      expression <- parseExp Lexer.minimumPrecedence
      expect Lexer.Semicolon
      return (Declaration lvalue (Just expression))
    Just Lexer.Semicolon -> do
      takeToken
      return (Declaration lvalue Nothing)
    others -> throwExpectTokensError "parseDeclaration" (Lexer.tokensFromList [Lexer.Assign, Lexer.Semicolon]) others

-- | Parse a statement
--
-- <statement> ::= "return" <exp> ";"
parseStatement :: Parser Statement
parseStatement = do
  nextToken <- peek
  case nextToken of
    Just Lexer.ReturnKeyword -> do
      takeToken
      returnVal <- parseExp Lexer.minimumPrecedence
      expect Lexer.Semicolon
      return (Return returnVal)
    Just Lexer.Semicolon -> do
      takeToken
      return Null
    Just _ -> do
      expression <- parseExp Lexer.minimumPrecedence
      expect Lexer.Semicolon
      return (Expression expression)
    Nothing -> throwExpectSomethingError "parseStatement"

-- | Parse a expression
--
-- <exp> ::= <factor> | <exp> <binop> <exp>
parseExp :: Lexer.Precedence -> Parser Exp
parseExp p =
  let loop _left _nextToken =
        if Lexer.mTIsBinary _nextToken && Lexer.mTPrecedenceGEt p _nextToken
          then do
            nleft <- case _nextToken of
              Just Lexer.Assign -> do
                takeToken
                right <- parseExp (Lexer.precedence (fromJust _nextToken))
                return $ Assignment _left right
              _ -> do
                operator <- parseBinop
                right <- parseExp (Lexer.precedence (fromJust _nextToken) + 1)
                return $ Binary operator _left right
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
    Just (Lexer.Identifier _) -> Var <$> parseIdentifier
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
      throwExpectTokensError
        "parseFactor"
        (Lexer.tokensFromList [Lexer.Constant 0, Lexer.Identifier "", Lexer.BitwiseComple, Lexer.Neg, Lexer.Not, Lexer.LeftParen])
        others

unaryOperatorMap :: M.Map Lexer.Token UnaryOperator
unaryOperatorMap =
  M.fromList
    [ (Lexer.BitwiseComple, Complement),
      (Lexer.Neg, Negate),
      (Lexer.Not, Not)
    ]

-- | Parse a unaryOperator
--
-- <unop> ::= "-" | "~" | "!"
parseUnop :: Parser UnaryOperator
parseUnop = do
  nextToken <- peek
  case nextToken >>= (`M.lookup` unaryOperatorMap) of
    Just uop -> do
      takeToken
      return uop
    _ ->
      throwExpectTokensError
        "parseUnop"
        (Lexer.tokensFromList (M.keys unaryOperatorMap))
        nextToken

binaryOperatorMap :: M.Map Lexer.Token BinaryOperator
binaryOperatorMap =
  M.fromList
    [ (Lexer.Neg, Subtract),
      (Lexer.Plus, Add),
      (Lexer.Mul, Multiply),
      (Lexer.Div, Divide),
      (Lexer.Rem, Remainder),
      (Lexer.And, And),
      (Lexer.Or, Or),
      (Lexer.TEQ, Equal),
      (Lexer.TNE, NotEqual),
      (Lexer.TLT, LessThan),
      (Lexer.TGT, GreaterThan),
      (Lexer.TLE, LessOrEqual),
      (Lexer.TGE, GreaterOrEqual)
    ]

-- | Parse a binaryOperator
--
-- <binop> ::= "-" | "+" | "*" | "/" | "%" | "&&" | "||"
--         | "==" | "!=" | "<" | "<=" | ">" | ">="
parseBinop :: Parser BinaryOperator
parseBinop = do
  nextToken <- peek
  case nextToken >>= (`M.lookup` binaryOperatorMap) of
    Just bop -> do
      takeToken
      return bop
    Nothing ->
      throwExpectTokensError
        "parseBinop"
        (Lexer.tokensFromList (M.keys binaryOperatorMap))
        nextToken

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
      throwExpectTokensError "parseIdentifier" (Lexer.tokensFromList [Lexer.Identifier ""]) others

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
      throwExpectTokensError "parseInt" (Lexer.tokensFromList [Lexer.Constant 0]) others
