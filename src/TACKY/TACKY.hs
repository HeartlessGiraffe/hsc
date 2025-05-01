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
    genProgram,
  )
where

import Control.Monad.State
import qualified Parser.Parser as Parser
import qualified Data.Sequence as Seq


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

returnTACKY :: (Val, TACKYGenState) -> Instructions
returnTACKY (val, TACKYGenState _ is) =
  appendInstruction is (Return val)

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

genProgram :: Parser.Program -> Program
genProgram (Parser.Program funcDef) = Program (genFuncDef funcDef)

genFuncDef :: Parser.FuncDef -> FuncDef
genFuncDef (Parser.Function name body) = Function (genIdentifier name) (genInstructions body)

genInstructions :: Parser.Statement -> Instructions
genInstructions (Parser.Return e) = returnTACKY (runState (emitTACKY e) initTACKYGenState)

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