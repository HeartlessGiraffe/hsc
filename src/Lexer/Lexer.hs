module Lexer.Lexer
  ( -- * 基本类型
    CodeString,
    Match,
    TokenRegex,

    -- * tokens and their regular expressions
    Token (..),
    emptyToken,
    singleToken,
    tokensFromList,
    dropToken,
    lookupToken,
    Precedence,
    isTBinary,
    minimumPrecedence,
    precedence,
    mTIsBinary,
    mTPrecedenceGEt,
    Tokens,
    LexerError (..),

    -- * Lexer
    lexer,
  )
where

import Data.Bifunctor (first)
import Data.Char (isSpace)
import Data.List (sortOn)
import Data.Maybe (fromJust, listToMaybe, mapMaybe)
import qualified Data.Ord as Ord
import Text.Read (readMaybe)
import Text.Regex.PCRE ((=~))
import qualified Data.Sequence as Seq

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
  | -- | 按位补
    TBitwiseComple
  | -- | 相反数
    TNeg
  | -- | 自减
    TDecre
  | -- | 加
    TPlus
  | -- | 乘
    TMul
  | -- | 除以
    TDiv
  | -- | 求余
    TRem
  deriving (Show, Eq)

-- | Tokens
type Tokens = Seq.Seq Token

-- | Empty Token
emptyToken :: Tokens 
emptyToken = Seq.empty

-- | Single Token
singleToken :: Token -> Tokens 
singleToken = Seq.singleton

-- | fromList
tokensFromList :: [Token] -> Tokens 
tokensFromList = Seq.fromList

-- | drop
dropToken :: Int -> Tokens -> Tokens 
dropToken = Seq.drop

-- | lookup
lookupToken :: Int -> Tokens -> Maybe Token 
lookupToken = Seq.lookup

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

-- | 按位补
bitwiseCompleRegex :: TokenRegex
bitwiseCompleRegex = "~"

-- | 相反数
negRegex :: TokenRegex
negRegex = "-"

-- | 加
plusRegex :: TokenRegex
plusRegex = "\\+"

-- | 乘
mulRegex :: TokenRegex
mulRegex = "\\*"

-- | 除以
divRegex :: TokenRegex
divRegex = "/"

-- | 求余
remRegex :: TokenRegex
remRegex = "%"

-- | 自减
decreRegex :: TokenRegex
decreRegex = "--"

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
          (semicolonRegex, TSemicolon),
          (bitwiseCompleRegex, TBitwiseComple),
          (negRegex, TNeg),
          (decreRegex, TDecre),
          (plusRegex, TPlus),
          (mulRegex, TMul),
          (divRegex, TDiv),
          (remRegex, TRem)
        ]

-- | Token是二元表达式
isTBinary :: Token -> Bool
isTBinary TNeg = True
isTBinary TPlus = True
isTBinary TMul = True
isTBinary TDiv = True
isTBinary TRem = True
isTBinary _ = False

-- | 优先级
type Precedence = Int

-- | 最低优先级
minimumPrecedence :: Precedence
minimumPrecedence = 0

-- | Token的优先级
precedence :: Token -> Precedence
precedence TPlus = 45
precedence TNeg = 45
precedence TMul = 50
precedence TDiv = 50
precedence TRem = 50
precedence _ = -1

-- | Token是二元操作符
mTIsBinary :: Maybe Token -> Bool
mTIsBinary mToken = (isTBinary <$> mToken) == Just True

-- | Token的优先级是否大于给定数
mTPrecedenceGEt :: Precedence -> Maybe Token -> Bool
mTPrecedenceGEt pre = maybe False (\x -> precedence x >= pre)

-- | 把单次匹配处理成 Token
toToken :: Match -> Token -> Token
toToken match (TIdentifier _) = identifierToKeyword $ TIdentifier match
toToken match (TConstant _) = TConstant (fromJust $ readMaybe match)
toToken _ t = t

-- | Token 长度
lenToken :: Token -> Int
lenToken (TIdentifier iStr) = length iStr
lenToken (TConstant cInt) = length (show cInt)
lenToken TIntKeyword = 3
lenToken TVoidKeyword = 4
lenToken TReturnKeyword = 6
lenToken TLeftParen = 1
lenToken TRightParen = 1
lenToken TLeftBrace = 1
lenToken TRightBrace = 1
lenToken TSemicolon = 1
lenToken TBitwiseComple = 1
lenToken TNeg = 1
lenToken TDecre = 2
lenToken TPlus = 1
lenToken TMul = 1
lenToken TDiv = 1
lenToken TRem = 1

-- | 如果标识符是关键字, 则将其视为关键字
identifierToKeyword :: Token -> Token
identifierToKeyword (TIdentifier name)
  | name == "int" = TIntKeyword
  | name == "void" = TVoidKeyword
  | name == "return" = TReturnKeyword
  | otherwise = TIdentifier name
identifierToKeyword token = token

-- | 针对代码文本, 遍历所有的Token对应的正则表达式, 找到匹配
-- 输出匹配到的Token和剩余的文本
findLongestMatch :: CodeString -> Maybe (Token, CodeString)
findLongestMatch input = findMaxLengthMatch $ mapMaybe findMatch tokenRegexes
  where
    findMatch :: (String, Token) -> Maybe (Token, CodeString)
    findMatch (regex, token) =
      let matches = input =~ regex
       in case matches of
            [] -> Nothing
            _ ->
              let match = head (head matches)
                  remaining = drop (length match) input
               in Just (toToken match token, remaining)
    findMaxLengthMatch :: [(Token, CodeString)] -> Maybe (Token, CodeString)
    findMaxLengthMatch matches = listToMaybe $ sortOn (Ord.Down . lenToken . fst) matches

-- | 词法分析器, 带有已处理的Token, 方便输出报错信息
lexerWithState :: CodeString -> Tokens -> Either LexerError Tokens
lexerWithState input = go (dropWhile isSpace input)
  where
    go "" accTokens = Right accTokens
    go remaining accTokens =
      case findLongestMatch remaining of
        Nothing -> Left $ LexerError "No match found" accTokens remaining
        Just (token, rest) -> go (dropWhile isSpace rest) (accTokens <> singleToken token)

-- | 词法分析器
lexer :: CodeString -> Either LexerError Tokens
lexer input = lexerWithState (dropWhile isSpace input) emptyToken
