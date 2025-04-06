module Lexer.Lexer
  ( -- * 基本类型
    CodeString,
    Match,
    TokenRegex,

    -- * tokens and their regular expressions
    Token (..),
    Tokens,
    LexerError (..),

    -- * Lexer
    lexer,
  )
where

import Data.Bifunctor (first)
import Data.Char (isSpace)
import Data.Foldable (Foldable (..))
import Data.Maybe (fromJust)
import Text.Read (readMaybe)
import Text.Regex.PCRE ((=~))

-- * 基本类型

-- | 代码字符串
type CodeString = String

-- | 单次匹配, 每个成功的单次匹配都会成为一个Token
type Match = String

-- | Token的正则表达式
type TokenRegex = String

-- * tokens and their regular expressions

-- | Token
data Token
  = -- | 标识符
    TIdentifier String
  | -- | 常量
    TConstant Int
  | -- | int 关键字
    TIntKeyword
  | -- | void 关键字
    TVoidKeyword
  | -- | return 关键字
    TReturnKeyword
  | -- | 左括号
    TLeftParen
  | -- | 右括号
    TRightParen
  | -- | 左大括号
    TLeftBrace
  | -- | 右大括号
    TRightBrace
  | -- | 分号
    TSemicolon
  deriving (Show, Eq)

-- | Tokens
type Tokens = [Token]

-- | Lexer error with context
data LexerError = LexerError
  { -- | Error message
    errorMessage :: String,
    -- | Tokens processed so far
    tokensSoFar :: Tokens,
    -- | Remaining unprocessed input
    remainingCode :: CodeString
  }
  deriving (Show, Eq)

-- | 标识符正则表达式
identifierRegex :: TokenRegex
identifierRegex = "[a-zA-Z_]\\w*\\b"

-- | 常量正则表达式
constantRegex :: TokenRegex
constantRegex = "[0-9]+\\b"

-- | int关键字正则表达式
intKeywordRegex :: TokenRegex
intKeywordRegex = "int\\b"

-- | void关键字正则表达式
voidKeywordRegex :: TokenRegex
voidKeywordRegex = "void\\b"

-- | return关键字正则表达式
returnKeywordRegex :: TokenRegex
returnKeywordRegex = "return\\b"

-- | 左括号正则表达式
leftParenRegex :: TokenRegex
leftParenRegex = "\\("

-- | 右括号正则表达式
rightParenRegex :: TokenRegex
rightParenRegex = "\\)"

-- | 左大括号正则表达式
leftBraceRegex :: TokenRegex
leftBraceRegex = "{"

-- | 右大括号正则表达式
rightBraceRegex :: TokenRegex
rightBraceRegex = "}"

-- | 分号正则表达式
semicolonRegex :: TokenRegex
semicolonRegex = ";"

-- | 开始
startRegex :: TokenRegex
startRegex = "\\A"

-- | token的正则表达式
tokenRegexes :: [(TokenRegex, Token)]
tokenRegexes =
  -- 需要从字符串开头开始匹配, 因此需要加上边界符号
  first (startRegex <>)
    <$> [ (identifierRegex, TIdentifier ""),
          (constantRegex, TConstant 0),
          (intKeywordRegex, TIntKeyword),
          (voidKeywordRegex, TVoidKeyword),
          (returnKeywordRegex, TReturnKeyword),
          (leftParenRegex, TLeftParen),
          (rightParenRegex, TRightParen),
          (leftBraceRegex, TLeftBrace),
          (rightBraceRegex, TRightBrace),
          (semicolonRegex, TSemicolon)
        ]

-- | 把单次匹配处理成 Token
toToken :: Match -> Token -> Token
toToken match (TIdentifier _) = identifierToKeyword $ TIdentifier match
toToken match (TConstant _) = TConstant (fromJust $ readMaybe match)
toToken _ TIntKeyword = TIntKeyword
toToken _ TVoidKeyword = TVoidKeyword
toToken _ TReturnKeyword = TReturnKeyword
toToken _ TLeftParen = TLeftParen
toToken _ TRightParen = TRightParen
toToken _ TLeftBrace = TLeftBrace
toToken _ TRightBrace = TRightBrace
toToken _ TSemicolon = TSemicolon

-- | 如果标识符是关键字, 则将其视为关键字
identifierToKeyword :: Token -> Token
identifierToKeyword (TIdentifier name)
  | name == "int" = TIntKeyword
  | name == "void" = TVoidKeyword
  | name == "return" = TReturnKeyword
  | otherwise = TIdentifier name
identifierToKeyword token = token

-- | 针对代码文本, 遍历所有的Token对应的正则表达式, 找到最长的匹配
-- 输出匹配到的Token和剩余的文本
findLongestMatch :: CodeString -> Maybe (Token, CodeString)
findLongestMatch input = foldl' findMatch Nothing tokenRegexes
  where
    findMatch :: Maybe (Token, CodeString) -> (String, Token) -> Maybe (Token, CodeString)
    findMatch acc (regex, token) =
      let matches = input =~ regex
       in case matches of
            [] -> acc
            _ ->
              let match = head (head matches)
                  remaining = drop (length match) input
               in Just (toToken match token, remaining)

-- | 词法分析器, 带有已处理的Token, 方便输出报错信息
lexerWithState :: CodeString -> [Token] -> Either LexerError [Token]
lexerWithState input = go (dropWhile isSpace input)
  where
    go "" accTokens = Right accTokens
    go remaining accTokens =
      case findLongestMatch remaining of
        Nothing -> Left $ LexerError "No match found" accTokens remaining
        Just (token, rest) -> go (dropWhile isSpace rest) (accTokens ++ [token])

-- | 词法分析器
lexer :: CodeString -> Either LexerError [Token]
lexer input = lexerWithState (dropWhile isSpace input) []
