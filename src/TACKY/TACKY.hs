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

    -- * TAKCY Generation

    -- ** TACKYState, including generating Names for temporary variables
    TACKYGenState (..),
    initTACKYGenState,
    instuctionAppend,
    TACKYGen,
    makeTemporary,

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
-- val = Constant(int) | Var(identifier)
-- unary_operator = Complement | Negate
-- binary_operator = Add | Subtract | Multiply | Divide | Remainder

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
  deriving (Show, Eq)

data TKVal = TKConstant Int | TKVar TKIdentifier
  deriving (Show, Eq)

isTKConstant :: TKVal -> Bool
isTKConstant (TKConstant _) = True
isTKConstant _ = False

data TKUnaryOperator = TKComplement | TKNegate
  deriving (Show, Eq)

data TKBinaryOperator = TKAdd | TKSubtract | TKMultiply | TKDivide | TKRemainder
  deriving (Show, Eq)

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

instance Pretty TKVal where
  pretty (TKConstant c) = PP.text "TKConstant " <> PP.text (show c)
  pretty (TKVar i) = PP.text "TKVar " <> pretty i

instance Pretty TKUnaryOperator where
  pretty TKComplement = PP.text "TKComplement"
  pretty TKNegate = PP.text "TKNegate"

instance Pretty TKBinaryOperator where
  pretty TKAdd = PP.text "TKAdd"
  pretty TKSubtract = PP.text "TKSubtract"
  pretty TKMultiply = PP.text "TKMultiply"
  pretty TKDivide = PP.text "TKDivide"
  pretty TKRemainder = PP.text "TKRemainder"

-- * TAKCY Generation

-- ** Generating Names for temporary variables

data TACKYGenState = TACKYGenState
  { tempVarCounter :: Int,
    currentInstructions :: [TKInstruction]
  }

initTACKYGenState :: TACKYGenState
initTACKYGenState = TACKYGenState 0 []

instuctionAppend :: TKInstruction -> TACKYGenState -> TACKYGenState
instuctionAppend i (TACKYGenState counter is) =
  TACKYGenState counter (is <> [i])

type TACKYGen = State TACKYGenState

makeTemporary :: TACKYGen TKIdentifier
makeTemporary = do
  i <- gets tempVarCounter
  instructions <- gets currentInstructions
  put (TACKYGenState (i + 1) instructions)
  return (TKIdentifier ("tmp." <> show i))

returnTACKY :: (TKVal, TACKYGenState) -> [TKInstruction]
returnTACKY (val, TACKYGenState _ is) =
  is <> [TKReturn val]

-- ** Generating TACKY

-- from:
--
-- program = Program(function_definition)
-- function_definition = Function(identifier name, statement body)
-- statement = Return(exp)
-- exp = Constant(int) | Unary(unary_operator, exp)
-- unary_operator = Complement | Negate
--
-- to:
--
-- program = Program(function_definition)
-- function_definition = Function(identifier, instruction* body)
-- instruction = Return(val)
--             | Unary(unary_operator, val src, val dst)
--             | Binary(binary_operator, val src1, val src2, val dst)
-- val = Constant(int) | Var(identifier)
-- unary_operator = Complement | Negate
-- binary_operator = Add | Subtract | Multiply | Divide | Remainder

genTKIdentifier :: Identifier -> TKIdentifier
genTKIdentifier (Identifier i) = TKIdentifier i

genTKProgram :: Program -> TKProgram
genTKProgram (Program funcDef) = TKProgram (genTKFuncDef funcDef)

genTKFuncDef :: FuncDef -> TKFuncDef
genTKFuncDef (Function name body) = TKFunction (genTKIdentifier name) (genTKInstructions body)

genTKInstructions :: Statement -> [TKInstruction]
genTKInstructions (Return e) = returnTACKY (runState (emitTAKCY e) initTACKYGenState)

emitTAKCY :: Exp -> TACKYGen TKVal
emitTAKCY (Constant c) =
  return (TKConstant c)
emitTAKCY (Unary op inner) = do
  src <- emitTAKCY inner
  dstName <- makeTemporary
  let dst = TKVar dstName
      tackyOp = genTKUnaryOperator op
  modify (instuctionAppend (TKUnary tackyOp src dst))
  return dst
emitTAKCY (Binary op e1 e2) = do
  v1 <- emitTAKCY e1
  v2 <- emitTAKCY e2
  dstName <- makeTemporary
  let dst = TKVar dstName
      tackyOp = genTKBinaryOperator op
  modify (instuctionAppend (TKBinary tackyOp v1 v2 dst))
  return dst

genTKUnaryOperator :: UnaryOperator -> TKUnaryOperator
genTKUnaryOperator Complement = TKComplement
genTKUnaryOperator Negate = TKNegate

genTKBinaryOperator :: BinaryOperator -> TKBinaryOperator
genTKBinaryOperator Add = TKAdd
genTKBinaryOperator Subtract = TKSubtract
genTKBinaryOperator Multiply = TKMultiply
genTKBinaryOperator Divide = TKDivide
genTKBinaryOperator Remainder = TKRemainder