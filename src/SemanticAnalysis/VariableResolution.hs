module SemanticAnalysis.VariableResolution
  ( resolveProgram,
  )
where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as M
import Parser.Parser

type FromCurrentBlock = Bool

type VariableMap = M.Map Identifier (Identifier, FromCurrentBlock)

setNotFromCurrentBlock :: VariableMap -> VariableMap
setNotFromCurrentBlock = fmap (\(i, _) -> (i, False))

data VariableResolutionState = VariableResolutionState
  { variableMap :: VariableMap,
    uniqNameCounter :: Int
  }
  deriving (Show)

data VRException
  = DuplicatedVariable Identifier
  | InvalidLValue Exp
  | UndeclaredVariable Identifier
  deriving (Show)

initVariableResolutionS :: VariableResolutionState
initVariableResolutionS = VariableResolutionState M.empty 0

type VariableResolution = StateT VariableResolutionState WithException

type WithException = Either VRException

getVariableMap :: VariableResolution VariableMap
getVariableMap = gets variableMap

putVariableMap :: VariableMap -> VariableResolution ()
putVariableMap vm = do 
  i <- gets uniqNameCounter
  put (VariableResolutionState vm i)

makeTemporary :: Identifier -> VariableResolution Identifier
makeTemporary (Identifier name) = do
  cnt <- gets uniqNameCounter
  vm <- getVariableMap
  put (VariableResolutionState vm (cnt + 1))
  return (Identifier (name <> ".localV" <> show cnt))

vmAdd :: Identifier -> Identifier -> FromCurrentBlock -> VariableResolution ()
vmAdd name uniqueName fromCurrentBlock = do
  vm <- getVariableMap
  cnt <- gets uniqNameCounter
  put
    ( VariableResolutionState
        (M.insert name (uniqueName, fromCurrentBlock) vm)
        cnt
    )

resolveProgram :: Program -> WithException Program
resolveProgram (Program funcDef) =
  Program <$> evalStateT (resolveFuncDef funcDef) initVariableResolutionS

resolveFuncDef :: FuncDef -> VariableResolution FuncDef
resolveFuncDef (Function name block) = Function name <$> resolveBlock block

resolveBlockItems ::
  (Traversable t) =>
  t BlockItem ->
  VariableResolution (t BlockItem)
resolveBlockItems = traverse resolveBlockItem

resolveBlock :: Block -> VariableResolution Block
resolveBlock (Block block) = do 
  b' <- resolveBlockItems block
  return (Block b')

resolveBlockItem :: BlockItem -> VariableResolution BlockItem
resolveBlockItem (S s) = S <$> resolveStatement s
resolveBlockItem (D d) = D <$> resolveDeclaration d

resolveDeclaration :: Declaration -> VariableResolution Declaration
resolveDeclaration (Declaration name mInit) = do
  vm <- getVariableMap
  case M.lookup name vm of 
    Just (_, True) -> throwError (DuplicatedVariable name)
    _ -> do
      uniqueName <- makeTemporary name
      vmAdd name uniqueName True
      i' <- case mInit of
        Just i -> Just <$> resolveExp i
        Nothing -> return Nothing
      return $ Declaration uniqueName i'

resolveStatement :: Statement -> VariableResolution Statement
resolveStatement (Return e) = Return <$> resolveExp e
resolveStatement (Expression e) = Expression <$> resolveExp e
resolveStatement Null = return Null
resolveStatement (If c t me) = do
  c' <- resolveExp c
  t' <- resolveStatement t
  e' <- case me of
    Just e -> Just <$> resolveStatement e
    Nothing -> return Nothing
  return $ If c' t' e'
resolveStatement (Compound block) = do 
  m' <- getVariableMap
  putVariableMap (setNotFromCurrentBlock m')
  b' <- resolveBlock block
  putVariableMap m'
  return $ Compound b'

resolveExp :: Exp -> VariableResolution Exp
resolveExp (Assignment exp1 exp2) =
  if notVar exp1
    then throwError (InvalidLValue exp1)
    else do
      exp1' <- resolveExp exp1
      exp2' <- resolveExp exp2
      return (Assignment exp1' exp2')
resolveExp (Var v) = do
  vm <- getVariableMap
  case M.lookup v vm of
    Just (uniqV, _) -> return (Var uniqV)
    Nothing -> throwError (UndeclaredVariable v)
resolveExp (Unary op e) =
  Unary op <$> resolveExp e
resolveExp (Binary op e1 e2) = do
  e1' <- resolveExp e1
  e2' <- resolveExp e2
  return (Binary op e1' e2')
resolveExp (Constant i) = return (Constant i)
resolveExp (Conditional c e1 e2) = do
  c' <- resolveExp c
  e1' <- resolveExp e1
  e2' <- resolveExp e2
  return (Conditional c' e1' e2')