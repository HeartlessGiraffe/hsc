module SemanticAnalysis.LoopLabeling
  ( LebelledProgram,
    LabelledFuncDef,
    LabelledBlock,
    LabelledBlockItems,
    LabelledBlockItem,
    LabelledStatement(..),
    labelProgram,
    labelProgramIO,
  )
where

import Control.Monad.Except
import Control.Monad.State
import qualified Parser.Parser as P
import Utils.Base (leftErrorIO)

type LabelledBlockItem = P.BlockItemWith LabelledStatement

type LabelledBlockItems = P.BlockItemsWith LabelledStatement

type LabelledBlock = P.BlockWith LabelledStatement

type LabelledFuncDef = P.FuncDefWith LabelledStatement

type LebelledProgram = P.ProgramWith LabelledStatement

data LabelledStatement
  = Return P.Exp
  | Expression P.Exp
  | If
      { _condition :: P.Exp,
        _then :: LabelledStatement,
        _else :: Maybe LabelledStatement
      }
  | Compound (P.BlockWith LabelledStatement)
  | Break
      { _label :: P.Identifier
      }
  | Continue
      { _label :: P.Identifier
      }
  | While
      { _condition :: P.Exp,
        _body :: LabelledStatement,
        _label :: P.Identifier
      }
  | DoWhile
      { _body :: LabelledStatement,
        _condition :: P.Exp,
        _label :: P.Identifier
      }
  | For
      { _fInit :: P.ForInit,
        _fCondition :: Maybe P.Exp,
        _fPost :: Maybe P.Exp,
        _fBody :: LabelledStatement,
        _label :: P.Identifier
      }
  | Null
  deriving (Show, Eq)

data LoopLabelState = LoopLebelState {
  currentLoopID :: Maybe Int 
  , loopCnt :: Int
}

initLoopLabelState :: LoopLabelState
initLoopLabelState = LoopLebelState Nothing 0

data LLException
  = BreakStatementOutsideOfLoop
  | ContinueStatementOutsideOfLoop
  deriving (Show)

type WithException = Either LLException

type LoopLabeling = StateT LoopLabelState WithException

getCurrentLID :: LoopLabeling (Maybe Int) 
getCurrentLID = gets currentLoopID

getCurrentLCnt :: LoopLabeling Int
getCurrentLCnt = gets loopCnt

putLID :: Maybe Int -> LoopLabeling ()
putLID lid = do 
  cnt <- getCurrentLCnt
  put (LoopLebelState lid cnt)

makeLoopLabel :: LoopLabeling Int
makeLoopLabel = do
  currentCnt <- getCurrentLCnt
  put (LoopLebelState (Just currentCnt) (currentCnt + 1))
  return currentCnt

makeIdentifierWithLID :: Int -> P.Identifier
makeIdentifierWithLID i = P.Identifier ("l.label" <> show i)

labelProgram :: P.Program -> WithException LebelledProgram
labelProgram (P.Program f) = P.Program <$> evalStateT (labelFuncDef f) initLoopLabelState

labelProgramIO :: P.Program -> IO LebelledProgram
labelProgramIO = leftErrorIO labelProgram

labelFuncDef :: P.FuncDef -> LoopLabeling LabelledFuncDef
labelFuncDef (P.Function name body) =
  P.Function name <$> labelBlock body

labelBlock :: P.Block -> LoopLabeling LabelledBlock
labelBlock (P.Block bis) = do
  bis' <- mapM labelBlockItem bis
  return (P.Block bis')

labelBlockItem :: P.BlockItem -> LoopLabeling LabelledBlockItem
labelBlockItem (P.S s) = P.S <$> labelStatement s
labelBlockItem (P.D d) = return (P.D d)

labelStatement :: P.Statement -> LoopLabeling LabelledStatement
labelStatement (P.Return e) = return $ Return e
labelStatement (P.Expression e) = return $ Expression e
labelStatement (P.If e s ms) = do
  s' <- labelStatement s
  ms' <- mapM labelStatement ms
  return $ If e s' ms'
labelStatement (P.Compound b) = Compound <$> labelBlock b
labelStatement P.Break = do
  currentLabel <- getCurrentLID
  case currentLabel of
    Nothing -> throwError BreakStatementOutsideOfLoop
    Just label -> return $ Break (makeIdentifierWithLID label)
labelStatement P.Continue = do
  currentLabel <- getCurrentLID
  case currentLabel of
    Nothing -> throwError ContinueStatementOutsideOfLoop
    Just label -> return $ Continue (makeIdentifierWithLID label)
labelStatement (P.While condE bodyS) = do
  lid <- getCurrentLID 
  newLabel <- makeLoopLabel
  bodyS' <- labelStatement bodyS
  putLID lid
  return $ While condE bodyS' (makeIdentifierWithLID newLabel)
labelStatement (P.DoWhile bodyS condE) = do
  lid <- getCurrentLID 
  newLabel <- makeLoopLabel
  bodyS' <- labelStatement bodyS
  putLID lid
  return $ DoWhile bodyS' condE (makeIdentifierWithLID newLabel)
labelStatement (P.For fInit condME1 postME2 bodyS) = do
  lid <- getCurrentLID 
  newLabel <- makeLoopLabel
  bodyS' <- labelStatement bodyS
  putLID lid
  return $ For fInit condME1 postME2 bodyS' (makeIdentifierWithLID newLabel)
labelStatement P.Null = return Null