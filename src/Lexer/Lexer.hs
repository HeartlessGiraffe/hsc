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
    lexerIO,
  )
where

import Data.Bifunctor (first)
import Data.Char (isSpace)
import Data.List (sortOn)
import Data.Maybe (fromJust, listToMaybe, mapMaybe)
import qualified Data.Ord as Ord
import qualified Data.Sequence as Seq
import Text.Read (readMaybe)
import Text.Regex.PCRE ((=~))
import Utils.Base

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
    Identifier String
  | -- | 常量
    Constant Int
  | -- | int 关键字
    IntKeyword
  | -- | void 关键字
    VoidKeyword
  | -- | return 关键字
    ReturnKeyword
  | -- | 左括号
    LeftParen
  | -- | 右括号
    RightParen
  | -- | 左大括号
    LeftBrace
  | -- | 右大括号
    RightBrace
  | -- | 分号
    Semicolon
  | -- | 按位补
    BitwiseComple
  | -- | 相反数
    Neg
  | -- | 自减
    Decre
  | -- | 加
    Plus
  | -- | 乘
    Mul
  | -- | 除以
    Div
  | -- | 求余
    Rem
  | -- | 逻辑非
    Not
  | -- | 逻辑与
    And
  | -- | 逻辑或
    Or
  | -- | 等于
    TEQ
  | -- | 不等于
    TNE
  | -- | 小于
    TLT
  | -- | 大于
    TGT
  | -- | 小于等于
    TLE
  | -- | 大于等于
    TGE
  | -- | 赋值
    Assign
  | -- | If
    IfKeyword
  | -- | Else
    ElseKeyword
  | -- | question mark in a conditional expression
    Question
  | -- | colon in a conditional expression
    Colon
  | DoKeyword
  | WhileKeyword
  | ForKeyword
  | BreakKeyword
  | ContinueKeyword
  deriving (Show, Eq, Ord)

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

-- | 逻辑非
notRegex :: TokenRegex
notRegex = "!"

-- | 逻辑与
andRegex :: TokenRegex
andRegex = "&&"

-- | 逻辑或
orRegex :: TokenRegex
orRegex = "\\|\\|"

-- | 等于
eqRegex :: TokenRegex
eqRegex = "=="

-- | 不等于
neqRegex :: TokenRegex
neqRegex = "!="

-- | 小于
ltRegex :: TokenRegex
ltRegex = "<"

-- | 大于
gtRegex :: TokenRegex
gtRegex = ">"

-- | 小于等于
leRegex :: TokenRegex
leRegex = "<="

-- | 大于等于
geRegex :: TokenRegex
geRegex = ">="

-- | 赋值
assignRegex :: TokenRegex
assignRegex = "="

ifKeywordRegex :: TokenRegex
ifKeywordRegex = "if"

elseKeywordRegex :: TokenRegex
elseKeywordRegex = "else"

questionRegex :: TokenRegex
questionRegex = "\\?"

colonRegex :: TokenRegex
colonRegex = "\\:"

doRegex :: TokenRegex
doRegex = "do"

whileRegex :: TokenRegex
whileRegex = "while"

forRegex :: TokenRegex
forRegex = "for"

breakRegex :: TokenRegex
breakRegex = "break"

continueRegex :: TokenRegex
continueRegex = "continue"

-- | token的正则表达式
tokenRegexes :: [(TokenRegex, Token)]
tokenRegexes =
  -- 需要从字符串开头开始匹配, 因此需要加上边界符号
  first (startRegex <>)
    <$> [ (identifierRegex, Identifier ""),
          (constantRegex, Constant 0),
          (intKeywordRegex, IntKeyword),
          (voidKeywordRegex, VoidKeyword),
          (returnKeywordRegex, ReturnKeyword),
          (leftParenRegex, LeftParen),
          (rightParenRegex, RightParen),
          (leftBraceRegex, LeftBrace),
          (rightBraceRegex, RightBrace),
          (semicolonRegex, Semicolon),
          (bitwiseCompleRegex, BitwiseComple),
          (negRegex, Neg),
          (decreRegex, Decre),
          (plusRegex, Plus),
          (mulRegex, Mul),
          (divRegex, Div),
          (remRegex, Rem),
          (notRegex, Not),
          (andRegex, And),
          (orRegex, Or),
          (eqRegex, TEQ),
          (neqRegex, TNE),
          (ltRegex, TLT),
          (gtRegex, TGT),
          (leRegex, TLE),
          (geRegex, TGE),
          (assignRegex, Assign),
          (ifKeywordRegex, IfKeyword),
          (elseKeywordRegex, ElseKeyword),
          (questionRegex, Question),
          (colonRegex, Colon),
          (doRegex, DoKeyword),
          (whileRegex, WhileKeyword),
          (forRegex, ForKeyword),
          (breakRegex, BreakKeyword),
          (continueRegex, ContinueKeyword)
        ]

-- | Token是二元表达式
isTBinary :: Token -> Bool
isTBinary Neg = True
isTBinary Plus = True
isTBinary Mul = True
isTBinary Div = True
isTBinary Rem = True
isTBinary Not = True
isTBinary And = True
isTBinary Or = True
isTBinary TEQ = True
isTBinary TNE = True
isTBinary TLT = True
isTBinary TGT = True
isTBinary TLE = True
isTBinary TGE = True
isTBinary Assign = True
isTBinary Question = True
isTBinary _ = False

-- | 优先级
type Precedence = Int

-- | 最低优先级
minimumPrecedence :: Precedence
minimumPrecedence = 0

-- | Token的优先级
precedence :: Token -> Precedence
precedence Plus = 45
precedence Neg = 45
precedence Mul = 50
precedence Div = 50
precedence Rem = 50
precedence TLT = 35
precedence TLE = 35
precedence TGT = 35
precedence TGE = 35
precedence TEQ = 30
precedence TNE = 30
precedence And = 10
precedence Or = 5
precedence Question = 3
precedence Assign = 1
precedence _ = -1

-- | Token是二元操作符
mTIsBinary :: Maybe Token -> Bool
mTIsBinary mToken = (isTBinary <$> mToken) == Just True

-- | Token的优先级是否大于给定数
mTPrecedenceGEt :: Precedence -> Maybe Token -> Bool
mTPrecedenceGEt pre = maybe False (\x -> precedence x >= pre)

-- | 把单次匹配处理成 Token
toToken :: Match -> Token -> Token
toToken match (Identifier _) = identifierToKeyword $ Identifier match
toToken match (Constant _) = Constant (fromJust $ readMaybe match)
toToken _ t = t

-- | Token 长度
lenToken :: Token -> Int
lenToken (Identifier iStr) = length iStr
lenToken (Constant cInt) = length (show cInt)
lenToken IntKeyword = 3
lenToken VoidKeyword = 4
lenToken ReturnKeyword = 6
lenToken LeftParen = 1
lenToken RightParen = 1
lenToken LeftBrace = 1
lenToken RightBrace = 1
lenToken Semicolon = 1
lenToken BitwiseComple = 1
lenToken Neg = 1
lenToken Decre = 2
lenToken Plus = 1
lenToken Mul = 1
lenToken Div = 1
lenToken Rem = 1
lenToken Not = 1
lenToken And = 2
lenToken Or = 2
lenToken TEQ = 2
lenToken TNE = 2
lenToken TGT = 1
lenToken TLT = 1
lenToken TGE = 2
lenToken TLE = 2
lenToken Assign = 1
lenToken IfKeyword = 2
lenToken ElseKeyword = 4
lenToken Question = 1
lenToken Colon = 1
lenToken DoKeyword = 2
lenToken WhileKeyword = 5
lenToken ForKeyword = 3
lenToken BreakKeyword = 5
lenToken ContinueKeyword = 8

-- | 如果标识符是关键字, 则将其视为关键字
identifierToKeyword :: Token -> Token
identifierToKeyword (Identifier name)
  | name == "int" = IntKeyword
  | name == "void" = VoidKeyword
  | name == "return" = ReturnKeyword
  | name == "if" = IfKeyword
  | name == "else" = ElseKeyword
  | name == "do" = DoKeyword
  | name == "while" = WhileKeyword
  | name == "for" = ForKeyword
  | name == "break" = BreakKeyword
  | name == "continue" = ContinueKeyword
  | otherwise = Identifier name
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

lexerIO :: CodeString -> IO Tokens
lexerIO = leftErrorIO lexer
