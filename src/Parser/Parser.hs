{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Parser.Parser
  ( -- * AST
    Identifier (..),
    ProgramWith (..),
    Program,
    BlockItemWith (..),
    BlockItem,
    BlockItemsWith,
    BlockWith (..),
    Block,
    Declaration (..),
    ForInit (..),
    FuncDefWith (..),
    FuncDef,
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
    evalParseIO,
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
import qualified Data.Sequence as Seq
import qualified Lexer.Lexer as Lexer
import Utils.Base (leftErrorIO)

-- * SC AST

-- ** AST Definition

--
-- program = Program(function_definition)
-- function_definition = Function(identifier name, block_item* body)
-- block_item = S(statement) | D(declaration)4
-- block = Block(block_item*)
-- declaration = Declaration(identifier name, exp? init)
-- for_init = InitDecl(declaration) | InitExp(exp?)
-- statement = Return(exp)
--           | Expression(exp)
--           | If(exp condition, statement then, statement? else)
--           | Compound(block)
--           | Break
--           | Continue
--           | While(exp condition, statement body)
--           | DoWhile(statement body, exp condition)
--           | For(for_init init, exp? condition, exp? post, statement body)
--           | Null
-- exp = Constant(int)
--     | Var(identifier)
--     | Unary(unary_operator, exp)
--     | Binary(binary_operator, exp, exp)
--     | Assignment(exp, exp)
--     | Conditional(exp condition, exp, exp)
-- unary_operator = Complement | Negate | Not
-- binary_operator = Add | Subtract | Multiply | Divide | Remainder | And | Or
--                 | Equal | NotEqual | LessThan | LessOrEqual
--                 | GreaterThan | GreaterOrEqual

newtype Identifier = Identifier
  { unIdenityfier :: String
  }
  deriving (Show, Eq, Ord)

data ProgramWith s = Program (FuncDefWith s)
  deriving (Show, Eq)

type Program = ProgramWith Statement

data FuncDefWith s = Function
  { _funcName :: Identifier,
    _funcBody :: BlockWith s
  }
  deriving (Show, Eq)

type FuncDef = FuncDefWith Statement

data BlockItemWith s = S s | D Declaration
  deriving (Show, Eq)

type BlockItem = BlockItemWith Statement

type BlockItemsWith s = Seq.Seq (BlockItemWith s)

type BlockItems = BlockItemsWith Statement

data BlockWith s = Block (BlockItemsWith s)
  deriving (Show, Eq)

type Block = BlockWith Statement

blockItemsAppend :: BlockItems -> BlockItem -> BlockItems
blockItemsAppend = (Seq.|>)

emptyBlockItem :: BlockItems
emptyBlockItem = Seq.empty

data Declaration = Declaration
  { _name :: Identifier,
    _init :: Maybe Exp
  }
  deriving (Show, Eq)

data ForInit = InitDecl Declaration | InitExp (Maybe Exp)
  deriving (Show, Eq)

data Statement
  = Return Exp
  | Expression Exp
  | If
      { _condition :: Exp,
        _then :: Statement,
        _else :: Maybe Statement
      }
  | Compound Block
  | Break
  | Continue
  | While
      { _condition :: Exp,
        _body :: Statement
      }
  | DoWhile
      { _body :: Statement,
        _condition :: Exp
      }
  | For
      { _fInit :: ForInit,
        _fCondition :: Maybe Exp,
        _fPost :: Maybe Exp,
        _fBody :: Statement
      }
  | Null
  deriving (Show, Eq)

data Exp
  = Constant Int
  | Var Identifier
  | Unary UnaryOperator Exp
  | Binary BinaryOperator Exp Exp
  | Assignment Exp Exp
  | Conditional Exp Exp Exp
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
-- <function> ::= "int" <identifier> "(" "void" ")" <block>
-- <block-item> ::= <statement> | <declaration>
-- <block> ::= "{" { <block-item> } "}"
-- <declaration> ::= "int" <identifier> [ "=" <exp> ] ";"
-- <statement> ::= "return" <exp> ";"
--               | <exp> ";"
--               | "if" "(" <exp> ")" <statement> [ "else" <statement> ]
--               | <block>
--               | ";"
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

evalParseIO :: Lexer.Tokens -> IO Program
evalParseIO = leftErrorIO evalParse

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
-- <function> ::= "int" <identifier> "(" "void" ")" <block>
parseFuncDef :: Parser FuncDef
parseFuncDef = do
  expect Lexer.IntKeyword
  functionName <- parseIdentifier
  expect Lexer.LeftParen
  expect Lexer.VoidKeyword
  expect Lexer.RightParen
  Function functionName <$> parseBlock

parseBlockItem :: Parser BlockItem
parseBlockItem = do
  nextToken <- peek
  case nextToken of
    Just Lexer.IntKeyword -> D <$> parseDeclaration
    _ -> S <$> parseStatement

-- | Block
--
-- <block> ::= "{" { <block-item> } "}"
parseBlock :: Parser Block
parseBlock = do
  expect Lexer.LeftBrace
  body <- loop emptyBlockItem
  expect Lexer.RightBrace
  return $ Block body
  where
    loop :: BlockItems -> Parser BlockItems
    loop functionBody = do
      nextToken <- peek
      case nextToken of
        Just Lexer.RightBrace -> return functionBody
        _ -> do
          nextBlockItem <- parseBlockItem
          loop (blockItemsAppend functionBody nextBlockItem)

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

-- | 一个helper函数, 用来Parse可能的exp
--
--  need to specify which token marks the end of the optional expression
-- [ <exp> ] token
parseMaybeExpEndWith :: Lexer.Token -> Parser (Maybe Exp)
parseMaybeExpEndWith token = do
  nextToken <- peek
  if nextToken == Just token
    then do
      takeToken
      return Nothing
    else do
      e <- parseExp Lexer.minimumPrecedence
      expect token
      return $ Just e

parseForInit :: Parser ForInit
parseForInit = do
  nextToken <- peek
  case nextToken of
    Just Lexer.IntKeyword -> InitDecl <$> parseDeclaration
    _ -> do
      me <- parseMaybeExpEndWith Lexer.Semicolon
      return $ InitExp me

-- | Parse a statement
--
-- <statement> ::= "return" <exp> ";"
--               | <exp> ";"
--               | "if" "(" <exp> ")" <statement> [ "else" <statement> ]
--               | <block>
--               | ";"
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
    Just Lexer.IfKeyword -> do
      takeToken
      expect Lexer.LeftParen
      e <- parseExp Lexer.minimumPrecedence
      expect Lexer.RightParen
      s <- parseStatement
      nToken <- peek
      case nToken of
        Just Lexer.ElseKeyword -> do
          takeToken
          If e s . Just <$> parseStatement
        _ -> return (If e s Nothing)
    Just Lexer.LeftBrace -> Compound <$> parseBlock
    Just Lexer.BreakKeyword -> do
      takeToken
      expect Lexer.Semicolon
      return Break
    Just Lexer.ContinueKeyword -> do
      takeToken
      expect Lexer.Semicolon
      return Continue
    Just Lexer.WhileKeyword -> do
      takeToken
      expect Lexer.LeftParen
      e <- parseExp Lexer.minimumPrecedence
      expect Lexer.RightParen
      While e <$> parseStatement
    Just Lexer.DoKeyword -> do
      takeToken
      s <- parseStatement
      expect Lexer.WhileKeyword
      expect Lexer.LeftParen
      e <- parseExp Lexer.minimumPrecedence
      expect Lexer.RightParen
      expect Lexer.Semicolon
      return $ DoWhile s e
    Just Lexer.ForKeyword -> do
      takeToken
      expect Lexer.LeftParen
      fi <- parseForInit
      me1 <- parseMaybeExpEndWith Lexer.Semicolon
      me2 <- parseMaybeExpEndWith Lexer.RightParen
      For fi me1 me2 <$> parseStatement
    Just _ -> do
      expression <- parseExp Lexer.minimumPrecedence
      expect Lexer.Semicolon
      return (Expression expression)
    Nothing -> throwExpectSomethingError "parseStatement"

parseConditionalMiddle :: Parser Exp
parseConditionalMiddle = do
  takeToken -- consume the ? token
  e <- parseExp Lexer.minimumPrecedence
  expect Lexer.Colon
  return e

-- | Parse a expression
--
-- <exp> ::= <factor> | <exp> <binop> <exp> | <exp> "?" <exp> ":" <exp>
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
              Just Lexer.Question -> do
                middle <- parseConditionalMiddle
                right <- parseExp (Lexer.precedence (fromJust _nextToken))
                return $ Conditional _left middle right
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
