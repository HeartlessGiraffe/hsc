{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

-- | An IR similar to TAC defined in the book Writing a C Compiler
module TACKY.TACKY
  ( -- * TACKY AST

    -- ** AST Definition
    Identifier (..),
    Program (..),
    FuncDef (..),
    Instruction (..),
    Val (..),
    isConstant,
    UnaryOperator (..),
    BinaryOperator (..),
    isRelationalOperator,

    -- * TAKCY Generation

    -- ** TACKYState, including generating Names for temporary variables
    TACKYGenState (..),
    initTACKYGenState,
    appendInst,
    TACKYGen,
    makeTmp,

    -- ** Generating TACKY
    genLProgram,
  )
where

import Control.Monad.State
import Data.Foldable (traverse_)
import qualified Data.Sequence as Seq
import qualified Parser.Parser as Parser
import qualified SemanticAnalysis.LoopLabeling as LL

-- * TACKY AST

-- ** AST Definition

-- program = Program(function_definition)
-- function_definition = Function(identifier, instruction* body)
-- instruction = Return(val)
--             | Unary(unary_operator, val src, val dst)
--             | Binary(binary_operator, val src1, val src2, val dst)
--             | Copy(val src, val dst)
--             | Jump(identifier target)
--             | JumpIfZero(val condition, identifier target)
--             | JumpIfNotZero(val condition, identifier target)
--             | Label(identifier)
-- val = Constant(int) | Var(identifier)
-- unary_operator = Complement | Negate | Not
-- binary_operator = Add | Subtract | Multiply | Divide | Remainder | Equal | NotEqual
--                 | LessThan | LessOrEqual | GreaterThan | GreaterOrEqual

newtype Identifier = Identifier
  {unIdentifier :: String}
  deriving (Show, Eq)

data Program = Program FuncDef deriving (Show, Eq)

data FuncDef = Function
  { _funcName :: Identifier,
    _funcBody :: Instructions
  }
  deriving (Show, Eq)

type Instructions = Seq.Seq Instruction

appendInstruction :: Instructions -> Instruction -> Instructions
appendInstruction instructions i = instructions Seq.|> i

concatInstructions :: Instructions -> Instructions -> Instructions
concatInstructions is1 is2 = is1 Seq.>< is2

emptyInstruction :: Instructions
emptyInstruction = Seq.empty

fromInstructionList :: [Instruction] -> Instructions
fromInstructionList = Seq.fromList

data Instruction
  = Return Val
  | Unary
      { _uoperator :: UnaryOperator,
        _src :: Val,
        _dst :: Val
      }
  | Binary
      { _boperator :: BinaryOperator,
        _src1 :: Val,
        _src2 :: Val,
        _dst :: Val
      }
  | Copy
      { _src :: Val,
        _dst :: Val
      }
  | Jump
      { _target :: Identifier
      }
  | JumpIfZero
      { _condition :: Val,
        _target :: Identifier
      }
  | JumpIfNotZero
      { _condition :: Val,
        _target :: Identifier
      }
  | Label Identifier
  deriving (Show, Eq)

data Val = Constant Int | Var Identifier
  deriving (Show, Eq)

isConstant :: Val -> Bool
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
  | Equal
  | NotEqual
  | LessThan
  | LessOrEqual
  | GreaterThan
  | GreaterOrEqual
  deriving (Show, Eq)

isRelationalOperator :: BinaryOperator -> Bool
isRelationalOperator Equal = True
isRelationalOperator NotEqual = True
isRelationalOperator LessThan = True
isRelationalOperator LessOrEqual = True
isRelationalOperator GreaterThan = True
isRelationalOperator GreaterOrEqual = True
isRelationalOperator _ = False

-- * TAKCY Generation

-- ** Generating Names for temporary variables

data TACKYGenState = TACKYGenState
  { tempVarCounter :: Int,
    currentInstructions :: Instructions
  }

initTACKYGenState :: TACKYGenState
initTACKYGenState = TACKYGenState 0 emptyInstruction

type TACKYGen = State TACKYGenState

appendInst :: Instruction -> TACKYGen ()
appendInst i = do
  TACKYGenState counter is <- get
  put (TACKYGenState counter (appendInstruction is i))

appendInsts :: [Instruction] -> TACKYGen ()
appendInsts instuctions = do
  TACKYGenState counter is <- get
  put (TACKYGenState counter (concatInstructions is (fromInstructionList instuctions)))

makeTmp :: TACKYGen Identifier
makeTmp = makeTmpWithName "tmp."

makeTmpWithName :: String -> TACKYGen Identifier
makeTmpWithName name = do
  i <- gets tempVarCounter
  instructions <- gets currentInstructions
  put (TACKYGenState (i + 1) instructions)
  return (Identifier (name <> show i))

-- ** Generating TACKY

-- from:
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
--
-- to:
--
-- program = Program(function_definition)
-- function_definition = Function(identifier, instruction* body)
-- instruction = Return(val)
--             | Unary(unary_operator, val src, val dst)
--             | Binary(binary_operator, val src1, val src2, val dst)
--             | Copy(val src, val dst)
--             | Jump(identifier target)
--             | JumpIfZero(val condition, identifier target)
--             | JumpIfNotZero(val condition, identifier target)
--             | Label(identifier)
-- val = Constant(int) | Var(identifier)
-- unary_operator = Complement | Negate | Not
-- binary_operator = Add | Subtract | Multiply | Divide | Remainder | Equal | NotEqual
--                 | LessThan | LessOrEqual | GreaterThan | GreaterOrEqual

genIdentifier :: Parser.Identifier -> Identifier
genIdentifier (Parser.Identifier i) = Identifier i

genLBlockItemInstructions :: LL.LabelledBlockItem -> TACKYGen ()
genLBlockItemInstructions (Parser.S s) = genLSInstructions s
genLBlockItemInstructions (Parser.D d) = genDInstructions d

genLBlockItemsInstructions ::
  (Traversable t) =>
  t LL.LabelledBlockItem ->
  TACKYGen ()
genLBlockItemsInstructions bis = do
  traverse_ genLBlockItemInstructions bis

genLBlockInstructions :: LL.LabelledBlock -> TACKYGen ()
genLBlockInstructions (Parser.Block bis) =
  genLBlockItemsInstructions bis

genLProgram :: LL.LebelledProgram -> Program
genLProgram (Parser.Program funcDef) =
  let fd = evalState (genLFuncDef funcDef) initTACKYGenState
   in Program fd

genLFuncDef :: LL.LabelledFuncDef -> TACKYGen FuncDef
genLFuncDef (Parser.Function name block) = do
  genLBlockInstructions block
  TACKYGenState _ is <- get
  let is' = appendInstruction is (Return (Constant 0))
  return $ Function (genIdentifier name) is'

genDInstructions :: Parser.Declaration -> TACKYGen ()
genDInstructions (Parser.Declaration name (Just e)) = do
  val <- emitTACKY e
  let var = Var (genIdentifier name)
  appendInst (Copy val var)
genDInstructions (Parser.Declaration _ Nothing) = return ()

genLSInstructions :: LL.LabelledStatement -> TACKYGen ()
genLSInstructions (LL.Return e) = do
  val <- emitTACKY e
  appendInst (Return val)
genLSInstructions (LL.Expression e) = do
  _ <- emitTACKY e
  return ()
genLSInstructions LL.Null = return ()
genLSInstructions (LL.If cond s1 Nothing) = do
  c <- emitTACKY cond
  label <- makeTmpWithName "if_end"
  appendInst (JumpIfZero c label)
  genLSInstructions s1
  appendInst (Label label)
genLSInstructions (LL.If cond s1 (Just s2)) = do
  c <- emitTACKY cond
  elseLabel <- makeTmpWithName "else"
  endLabel <- makeTmpWithName "if_end"
  appendInst (JumpIfZero c elseLabel)
  genLSInstructions s1
  appendInst (Jump endLabel)
  appendInst (Label elseLabel)
  genLSInstructions s2
  appendInst (Label endLabel)
genLSInstructions (LL.Compound block) = do
  genLBlockInstructions block
genLSInstructions (LL.Break label) = do
  appendInst (Jump (makeBreakLabel label))
genLSInstructions (LL.Continue label) = do
  appendInst (Jump (makeContinueLabel label))
genLSInstructions (LL.DoWhile sBody eCond label) = do
  startLabel <- makeTmpWithName "do_while_start"
  appendInst (Label startLabel)
  genLSInstructions sBody
  appendInst (Label (makeContinueLabel label))
  v <- emitTACKY eCond
  appendInst (JumpIfNotZero v startLabel)
  appendInst (Label (makeBreakLabel label))
genLSInstructions (LL.While eCond sBody label) = do
  let continueLabel = makeContinueLabel label
      breakLabel = makeBreakLabel label
  appendInst (Label continueLabel)
  v <- emitTACKY eCond
  appendInst (JumpIfZero v breakLabel)
  genLSInstructions sBody
  appendInst (Jump continueLabel)
  appendInst (Label breakLabel)
genLSInstructions (LL.For fInit meCond mePost sBody label) = do
  let continueLabel = makeContinueLabel label
      breakLabel = makeBreakLabel label
  startLabel <- makeTmpWithName "for_start"
  genForInitInstructions fInit
  appendInst (Label startLabel)
  mv1 <- emitMTACKY meCond
  appendInst (JumpIfZero (makeForControllingEVal mv1) breakLabel)
  genLSInstructions sBody
  appendInst (Label continueLabel)
  _ <- emitMTACKY mePost
  appendInst (Jump startLabel)
  appendInst (Label breakLabel)

makeForControllingEVal :: Maybe Val -> Val 
makeForControllingEVal (Just v) = v 
makeForControllingEVal Nothing = Constant 1

genForInitInstructions :: Parser.ForInit -> TACKYGen ()
genForInitInstructions (Parser.InitDecl d) = genDInstructions d
genForInitInstructions (Parser.InitExp me) = do 
  _ <- emitMTACKY me
  return ()

makeBreakLabel :: Parser.Identifier -> Identifier
makeBreakLabel (Parser.Identifier name) = Identifier $ "break_" <> name

makeContinueLabel :: Parser.Identifier -> Identifier
makeContinueLabel (Parser.Identifier name) = Identifier $ "continue_" <> name

emitMTACKY :: Maybe Parser.Exp -> TACKYGen (Maybe Val)
emitMTACKY (Just e) = Just <$> emitTACKY e
emitMTACKY Nothing = return Nothing

emitTACKY :: Parser.Exp -> TACKYGen Val
emitTACKY (Parser.Constant c) =
  return (Constant c)
emitTACKY (Parser.Unary op inner) = do
  src <- emitTACKY inner
  dstName <- makeTmp
  let dst = Var dstName
      tackyOp = genUnaryOperator op
  appendInst (Unary tackyOp src dst)
  return dst
emitTACKY (Parser.Binary Parser.And e1 e2) = do
  v1 <- emitTACKY e1
  falseLabel <- makeTmpWithName "and_false"
  endLabel <- makeTmpWithName "and_end"
  appendInst (JumpIfZero v1 falseLabel)
  v2 <- emitTACKY e2
  appendInst (JumpIfZero v2 falseLabel)
  resName <- makeTmp
  let res = Var resName
  appendInsts
    [ Copy (Constant 1) res,
      Jump endLabel,
      Label falseLabel,
      Copy (Constant 0) res,
      Label endLabel
    ]
  return res
emitTACKY (Parser.Binary Parser.Or e1 e2) = do
  v1 <- emitTACKY e1
  trueLabel <- makeTmpWithName "or_true"
  endLabel <- makeTmpWithName "or_end"
  appendInst (JumpIfNotZero v1 trueLabel)
  v2 <- emitTACKY e2
  appendInst (JumpIfNotZero v2 trueLabel)
  resName <- makeTmp
  let res = Var resName
  appendInsts
    [ Copy (Constant 0) res,
      Jump endLabel,
      Label trueLabel,
      Copy (Constant 1) res,
      Label endLabel
    ]
  return res
emitTACKY (Parser.Binary op e1 e2) = do
  v1 <- emitTACKY e1
  v2 <- emitTACKY e2
  dstName <- makeTmp
  let dst = Var dstName
      tackyOp = genBinaryOperator op
  appendInst (Binary tackyOp v1 v2 dst)
  return dst
emitTACKY (Parser.Var v) = return $ Var (genIdentifier v)
emitTACKY (Parser.Assignment (Parser.Var v) rhs) = do
  result <- emitTACKY rhs
  let var' = Var (genIdentifier v)
  appendInst (Copy result var')
  return var'
emitTACKY (Parser.Assignment lhs _) =
  error $ "emitTACKY: lhs" ++ show lhs ++ " is not a variable !? It should have been resolved!"
emitTACKY (Parser.Conditional cond e1 e2) = do
  c <- emitTACKY cond
  e2Label <- makeTmpWithName "cond_e2_label"
  end <- makeTmpWithName "cond_end"
  resName <- makeTmp
  let res = Var resName
  appendInst (JumpIfZero c e2Label)
  v1 <- emitTACKY e1
  appendInst (Copy v1 res)
  appendInst (Jump end)
  appendInst (Label e2Label)
  v2 <- emitTACKY e2
  appendInst (Copy v2 res)
  appendInst (Label end)
  return res

genUnaryOperator :: Parser.UnaryOperator -> UnaryOperator
genUnaryOperator Parser.Complement = Complement
genUnaryOperator Parser.Negate = Negate
genUnaryOperator Parser.Not = Not

genBinaryOperator :: Parser.BinaryOperator -> BinaryOperator
genBinaryOperator Parser.Add = Add
genBinaryOperator Parser.Subtract = Subtract
genBinaryOperator Parser.Multiply = Multiply
genBinaryOperator Parser.Divide = Divide
genBinaryOperator Parser.Remainder = Remainder
genBinaryOperator Parser.Equal = Equal
genBinaryOperator Parser.NotEqual = NotEqual
genBinaryOperator Parser.GreaterThan = GreaterThan
genBinaryOperator Parser.GreaterOrEqual = GreaterOrEqual
genBinaryOperator Parser.LessThan = LessThan
genBinaryOperator Parser.LessOrEqual = LessOrEqual
genBinaryOperator Parser.And = error "genBinaryOperator: shouldnt convert And"
genBinaryOperator Parser.Or = error "genBinaryOperator: shouldnt convert Or"