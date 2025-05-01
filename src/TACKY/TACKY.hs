{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

-- | An IR similar to TAC defined in the book Writing a C Compiler
module TACKY.TACKY
  ( -- * TACKY AST

    -- ** AST Definition
    TKIdentifier (..),
    TKProgram (..),
    TKFuncDef (..),
    TKInstruction (..),
    TKVal (..),
    TKUnaryOperator (..),
    TKBinaryOperator (..),
    isRelationalOperator,

    -- * TAKCY Generation

    -- ** TACKYState, including generating Names for temporary variables
    TACKYGenState (..),
    initTACKYGenState,
    appendInst,
    TACKYGen,
    makeTmp,

    -- ** Generating TACKY
    genTKProgram,
  )
where

import Control.Monad.State
import Parser.Parser
import qualified Text.PrettyPrint as PP
import Utils (Pretty (..))

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

newtype TKIdentifier = TKIdentifier
  {unTKIdentifier :: String}
  deriving (Show, Eq)

data TKProgram = TKProgram TKFuncDef deriving (Show, Eq)

data TKFuncDef = TKFunction
  { _funcName :: TKIdentifier,
    _funcBody :: [TKInstruction]
  }
  deriving (Show, Eq)

data TKInstruction
  = TKReturn TKVal
  | TKUnary
      { _uoperator :: TKUnaryOperator,
        _src :: TKVal,
        _dst :: TKVal
      }
  | TKBinary
      { _boperator :: TKBinaryOperator,
        _src1 :: TKVal,
        _src2 :: TKVal,
        _dst :: TKVal
      }
  | TKCopy
      { _src :: TKVal,
        _dst :: TKVal
      }
  | TKJump
      { _target :: TKIdentifier
      }
  | TKJumpIfZero
      { _condition :: TKVal,
        _target :: TKIdentifier
      }
  | TKJumpIfNotZero
      { _condition :: TKVal,
        _target :: TKIdentifier
      }
  | TKLabel TKIdentifier
  deriving (Show, Eq)

data TKVal = TKConstant Int | TKVar TKIdentifier
  deriving (Show, Eq)

isTKConstant :: TKVal -> Bool
isTKConstant (TKConstant _) = True
isTKConstant _ = False

data TKUnaryOperator = TKComplement | TKNegate | TKNot
  deriving (Show, Eq)

data TKBinaryOperator
  = TKAdd
  | TKSubtract
  | TKMultiply
  | TKDivide
  | TKRemainder
  | TKEqual
  | TKNotEqual
  | TKLessThan
  | TKLessOrEqual
  | TKGreaterThan
  | TKGreaterOrEqual
  deriving (Show, Eq)

isRelationalOperator :: TKBinaryOperator -> Bool 
isRelationalOperator TKEqual = True 
isRelationalOperator TKNotEqual = True 
isRelationalOperator TKLessThan = True 
isRelationalOperator TKLessOrEqual = True 
isRelationalOperator TKGreaterThan = True 
isRelationalOperator TKGreaterOrEqual = True 
isRelationalOperator _ = False

-- ** Pretty

instance Pretty TKIdentifier where
  pretty (TKIdentifier name) = PP.text name

instance Pretty TKProgram where
  pretty (TKProgram funcDef) =
    PP.text "TKProgram {" PP.$$ PP.nest 2 (pretty funcDef) PP.$$ PP.text "}"

instance Pretty TKFuncDef where
  pretty (TKFunction name body) =
    PP.text "TKFunction (" <> pretty name <> PP.text ") {" PP.$$ PP.nest 2 (PP.sep (pretty <$> body)) PP.$$ PP.text "}"

instance Pretty TKInstruction where
  pretty (TKReturn expr) =
    PP.text "TKReturn(" <> pretty expr <> PP.text ")"
  pretty (TKUnary op src dst) =
    PP.text "TKUnary(" <> pretty op <> PP.text ", " <> pretty src <> PP.text ", " <> pretty dst <> PP.text ")"
  pretty (TKBinary op src1 src2 dst) =
    if isTKConstant src1 && isTKConstant src2 && isTKConstant dst
      then
        PP.text "TKBinary(" <> pretty op <> PP.text ", " <> pretty src1 <> PP.text ", " <> pretty src2 <> PP.text ", " <> pretty dst <> PP.text ")"
      else PP.text "TKBinary(" PP.$$ PP.nest 2 (pretty op) <> PP.text ", " PP.$$ PP.nest 2 (pretty src1) PP.$$ PP.nest 2 (pretty src2) <> PP.text ", " PP.$$ PP.nest 2 (pretty dst) PP.$$ PP.text ")"
  pretty (TKCopy src dst) =
    PP.text "TKCopy(" <> pretty src <> PP.text ", " <> pretty dst <> PP.text ")"
  pretty (TKJump target) =
    PP.text "TKJump(" <> pretty target <> PP.text ")"
  pretty (TKJumpIfZero cond target) =
    PP.text "TKJumpIfZero(" <> pretty cond <> PP.text ", " <> pretty target <> PP.text ")"
  pretty (TKJumpIfNotZero cond target) =
    PP.text "TKJumpIfNotZero(" <> pretty cond <> PP.text ", " <> pretty target <> PP.text ")"
  pretty (TKLabel label) =
    PP.text "TKLabel(" <> pretty label <> PP.text ")"

instance Pretty TKVal where
  pretty (TKConstant c) = PP.text "TKConstant " <> PP.text (show c)
  pretty (TKVar i) = PP.text "TKVar " <> pretty i

instance Pretty TKUnaryOperator where
  pretty TKComplement = PP.text "TKComplement"
  pretty TKNegate = PP.text "TKNegate"
  pretty TKNot = PP.text "TKNot"

instance Pretty TKBinaryOperator where
  pretty TKAdd = PP.text "TKAdd"
  pretty TKSubtract = PP.text "TKSubtract"
  pretty TKMultiply = PP.text "TKMultiply"
  pretty TKDivide = PP.text "TKDivide"
  pretty TKRemainder = PP.text "TKRemainder"
  pretty TKEqual = PP.text "TKEqual"
  pretty TKNotEqual = PP.text "TKNotEqual"
  pretty TKGreaterThan = PP.text "TKGreaterThan"
  pretty TKGreaterOrEqual = PP.text "TKGreaterOrEqual"
  pretty TKLessThan = PP.text "TKLessThan"
  pretty TKLessOrEqual = PP.text "TKLessOrEqual"

-- * TAKCY Generation

-- ** Generating Names for temporary variables

data TACKYGenState = TACKYGenState
  { tempVarCounter :: Int,
    currentInstructions :: [TKInstruction]
  }

initTACKYGenState :: TACKYGenState
initTACKYGenState = TACKYGenState 0 []

type TACKYGen = State TACKYGenState

appendInst :: TKInstruction -> TACKYGen ()
appendInst i = do
  TACKYGenState counter is <- get
  put (TACKYGenState counter (is <> [i]))

appendInsts :: [TKInstruction] -> TACKYGen ()
appendInsts instuctions = do
  TACKYGenState counter is <- get
  put (TACKYGenState counter (is <> instuctions))

makeTmp :: TACKYGen TKIdentifier
makeTmp = makeTmpWithName "tmp."

makeTmpWithName :: String -> TACKYGen TKIdentifier
makeTmpWithName name = do
  i <- gets tempVarCounter
  instructions <- gets currentInstructions
  put (TACKYGenState (i + 1) instructions)
  return (TKIdentifier (name <> show i))

returnTACKY :: (TKVal, TACKYGenState) -> [TKInstruction]
returnTACKY (val, TACKYGenState _ is) =
  is <> [TKReturn val]

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

genTKIdentifier :: Identifier -> TKIdentifier
genTKIdentifier (Identifier i) = TKIdentifier i

genTKProgram :: Program -> TKProgram
genTKProgram (Program funcDef) = TKProgram (genTKFuncDef funcDef)

genTKFuncDef :: FuncDef -> TKFuncDef
genTKFuncDef (Function name body) = TKFunction (genTKIdentifier name) (genTKInstructions body)

genTKInstructions :: Statement -> [TKInstruction]
genTKInstructions (Return e) = returnTACKY (runState (emitTACKY e) initTACKYGenState)

emitTACKY :: Exp -> TACKYGen TKVal
emitTACKY (Constant c) =
  return (TKConstant c)
emitTACKY (Unary op inner) = do
  src <- emitTACKY inner
  dstName <- makeTmp
  let dst = TKVar dstName
      tackyOp = genTKUnaryOperator op
  appendInst (TKUnary tackyOp src dst)
  return dst
emitTACKY (Binary And e1 e2) = do
  v1 <- emitTACKY e1
  falseLabel <- makeTmpWithName "and_false"
  endLabel <- makeTmpWithName "and_end"
  appendInst (TKJumpIfZero v1 falseLabel)
  v2 <- emitTACKY e2
  appendInst (TKJumpIfZero v2 falseLabel)
  resName <- makeTmp
  let res = TKVar resName
  appendInsts
    [ TKCopy (TKConstant 1) res,
      TKJump endLabel,
      TKLabel falseLabel,
      TKCopy (TKConstant 0) res,
      TKLabel endLabel
    ]
  return res
emitTACKY (Binary Or e1 e2) = do
  v1 <- emitTACKY e1
  trueLabel <- makeTmpWithName "or_true"
  endLabel <- makeTmpWithName "or_end"
  appendInst (TKJumpIfNotZero v1 trueLabel)
  v2 <- emitTACKY e2
  appendInst (TKJumpIfNotZero v2 trueLabel)
  resName <- makeTmp
  let res = TKVar resName
  appendInsts
    [ TKCopy (TKConstant 0) res,
      TKJump endLabel,
      TKLabel trueLabel,
      TKCopy (TKConstant 1) res,
      TKLabel endLabel
    ]
  return res
emitTACKY (Binary op e1 e2) = do
  v1 <- emitTACKY e1
  v2 <- emitTACKY e2
  dstName <- makeTmp
  let dst = TKVar dstName
      tackyOp = genTKBinaryOperator op
  appendInst (TKBinary tackyOp v1 v2 dst)
  return dst

genTKUnaryOperator :: UnaryOperator -> TKUnaryOperator
genTKUnaryOperator Complement = TKComplement
genTKUnaryOperator Negate = TKNegate
genTKUnaryOperator Not = TKNot

genTKBinaryOperator :: BinaryOperator -> TKBinaryOperator
genTKBinaryOperator Add = TKAdd
genTKBinaryOperator Subtract = TKSubtract
genTKBinaryOperator Multiply = TKMultiply
genTKBinaryOperator Divide = TKDivide
genTKBinaryOperator Remainder = TKRemainder
genTKBinaryOperator Equal = TKEqual
genTKBinaryOperator NotEqual = TKNotEqual
genTKBinaryOperator GreaterThan = TKGreaterThan
genTKBinaryOperator GreaterOrEqual = TKGreaterOrEqual
genTKBinaryOperator LessThan = TKLessThan
genTKBinaryOperator LessOrEqual = TKLessOrEqual
genTKBinaryOperator And = error "genTKBinaryOperator: shouldnt convert And"
genTKBinaryOperator Or = error "genTKBinaryOperator: shouldnt convert Or"