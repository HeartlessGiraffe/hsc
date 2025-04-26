{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module AssemblyGen.AssemblyGen
  ( -- * Assembly represented By ADT
    AProgram (..),
    AFuncDef (..),
    AIdentifier (..),
    AInstruction (..),
    AUnaryOperator (..),
    ABinaryOperator (..),
    AOperand (..),
    AReg (..),

    -- * Converting
    convertProgram,
    convertProgramWithReplacePseudoRegs,
    convertProgramWithFixedInstructions,
  )
where

-- convertProgram,
-- convertFunction,
-- convertReturn,
-- convertConstant,
-- convertIdentifier,

import Control.Monad.State
import qualified Data.Map as M
import TACKY.TACKY
import qualified Text.PrettyPrint as PP
import Utils (Pretty (..))

-- * Assembly represented By ADT

-- ** AST Definition

-- program = Program(function_definition)
-- function_definition = Function(identifier name, instruction* instructions)
-- instruction = Mov(operand src, operand dst)
--             | Unary(unary_operator, operand)
--             | Binary(binary_operator, operand, operand)
--             | Idiv(operand)
--             | Cdq
--             | AllocateStack(int)
--             | Ret
-- unary_operator = Neg | Not
-- binary_operator = Add | Sub | Mult
-- operand = Imm(int) | Reg(reg) | Pseudo(identifier) | Stack(int)
-- reg = AX | DX | R10 | R11

data AProgram = AProgram AFuncDef
  deriving (Show, Eq)

data AFuncDef = AFunction
  { _aFuncName :: AIdentifier,
    _aFuncInstructions :: [AInstruction]
  }
  deriving (Show, Eq)

newtype AIdentifier = AIdentifier
  { unAIdentifier :: String
  }
  deriving (Show, Eq, Ord)

data AInstruction
  = AMov
      { _aMovSrc :: AOperand,
        _aMovDst :: AOperand
      }
  | AUnary AUnaryOperator AOperand
  | ABinary ABinaryOperator AOperand AOperand
  | AIdiv AOperand
  | ACdq
  | AAllocateStack Int
  | ARet
  deriving (Show, Eq)

data AUnaryOperator = ANeg | ANot
  deriving (Show, Eq)

data ABinaryOperator = AAdd | ASub | AMult
  deriving (Show, Eq)

data AOperand = AImm Int | ARegister AReg | APseudo AIdentifier | AStack Int
  deriving (Show, Eq)

data AReg = AX | DX | R10 | R11
  deriving (Show, Eq)

-- **  Pretty Printing

instance Pretty AProgram where
  pretty (AProgram funcDef) =
    PP.text "AProgram {" PP.$$ PP.nest 2 (pretty funcDef) PP.$$ PP.text "}"

instance Pretty AFuncDef where
  pretty (AFunction name instructions) =
    PP.text "AFunction {" PP.$$ PP.nest 2 (pretty name) PP.$$ PP.nest 2 (PP.vcat (map pretty instructions)) PP.$$ PP.text "}"

instance Pretty AIdentifier where
  pretty (AIdentifier name) = PP.text name

instance Pretty AInstruction where
  pretty (AMov src dst) =
    PP.text "AMov" PP.<+> (pretty src <> PP.text ",") PP.<+> pretty dst
  pretty (AUnary operator oprand) =
    PP.text "AUnary" PP.<+> (pretty operator <> PP.text ",") PP.<+> pretty oprand
  pretty (ABinary operator oprand1 oprand2) =
    PP.text "ABinary" PP.<+> (pretty operator <> PP.text ",") PP.<+> (pretty oprand1 <> PP.text ",") PP.<+> pretty oprand2
  pretty (AIdiv operator) =
    PP.text "AIdiv" PP.<+> pretty operator
  pretty ACdq =
    PP.text "ACdq"
  pretty (AAllocateStack i) =
    PP.text "AAllocateStack" PP.<+> PP.int i
  pretty ARet = PP.text "ARet"

instance Pretty AOperand where
  pretty (AImm i) = PP.text "AImm" PP.<+> PP.int i
  pretty (ARegister reg) = PP.text "ARegister" PP.<+> pretty reg
  pretty (APseudo i) = PP.text "APseudo" PP.<+> pretty i
  pretty (AStack i) = PP.text "AStack" PP.<+> PP.int i

instance Pretty AUnaryOperator where
  pretty ANeg = PP.text "ANeg"
  pretty ANot = PP.text "ANot"

instance Pretty ABinaryOperator where
  pretty AAdd = PP.text "AAdd"
  pretty ASub = PP.text "ASub"
  pretty AMult = PP.text "AMult"

instance Pretty AReg where
  pretty AX = PP.text "AX"
  pretty DX = PP.text "DX"
  pretty R10 = PP.text "R10"
  pretty R11 = PP.text "R11"

-- * Converting

-- from:
-- program = Program(function_definition)
-- function_definition = Function(identifier name, statement body)
-- statement = Return(exp)
-- exp = Constant(int)
--     | Unary(unary_operator, exp)
--     | Binary(binary_operator, exp, exp)
-- unary_operator = Complement | Negate
-- binary_operator = Add | Subtract | Multiply | Divide | Remainder
--
-- to:
-- program = Program(function_definition)
-- function_definition = Function(identifier name, instruction* instructions)
-- instruction = Mov(operand src, operand dst)
--             | Unary(unary_operator, operand)
--             | Binary(binary_operator, operand, operand)
--             | Idiv(operand)
--             | Cdq
--             | AllocateStack(int)
--             | Ret
-- unary_operator = Neg | Not
-- binary_operator = Add | Sub | Mult
-- operand = Imm(int) | Reg(reg) | Pseudo(identifier) | Stack(int)
-- reg = AX | DX | R10 | R11
convertProgram :: TKProgram -> AProgram
convertProgram (TKProgram funcDef) = AProgram (convertFunction funcDef)

convertFunction :: TKFuncDef -> AFuncDef
convertFunction (TKFunction name instuctions) =
  AFunction (convertIdentifier name) (concatMap convertInstruction instuctions)

convertInstruction :: TKInstruction -> [AInstruction]
convertInstruction (TKReturn val) =
  [ AMov (convertVal val) (ARegister AX),
    ARet
  ]
convertInstruction (TKUnary uOperator src dst) =
  [ AMov (convertVal src) (convertVal dst),
    AUnary (convertUnaryOperator uOperator) (convertVal dst)
  ]
convertInstruction (TKBinary TKDivide src1 src2 dst) =
  [ AMov (convertVal src1) (ARegister AX),
    ACdq,
    AIdiv (convertVal src2),
    AMov (ARegister AX) (convertVal dst)
  ]
convertInstruction (TKBinary TKRemainder src1 src2 dst) =
  [ AMov (convertVal src1) (ARegister AX),
    ACdq,
    AIdiv (convertVal src2),
    AMov (ARegister DX) (convertVal dst)
  ]
convertInstruction (TKBinary bOperator src1 src2 dst) =
  [ AMov (convertVal src1) (convertVal dst),
    ABinary (convertBinaryOperator bOperator) (convertVal src2) (convertVal dst)
  ]

convertIdentifier :: TKIdentifier -> AIdentifier
convertIdentifier (TKIdentifier name) = AIdentifier name

convertUnaryOperator :: TKUnaryOperator -> AUnaryOperator
convertUnaryOperator TKComplement = ANot
convertUnaryOperator TKNegate = ANeg

convertBinaryOperator :: TKBinaryOperator -> ABinaryOperator
convertBinaryOperator TKAdd = AAdd
convertBinaryOperator TKSubtract = ASub
convertBinaryOperator TKMultiply = AMult
convertBinaryOperator op = error $ "convertBinaryOperator: " ++ show op ++ " shouldn't be convert!"

convertVal :: TKVal -> AOperand
convertVal (TKConstant i) = AImm i
convertVal (TKVar i) = APseudo (convertIdentifier i)

-- * replacing pseudoregisters

type Offset = Int

newtype IdentifierOffsets = IdentifierOffsets
  { unIdentifierOffsets :: M.Map AIdentifier Offset
  }

addToTable :: AIdentifier -> Offset -> IdentifierOffsets -> IdentifierOffsets
addToTable identifier offset (IdentifierOffsets table) =
  IdentifierOffsets $ M.insert identifier offset table

initIdentifierOffsets :: IdentifierOffsets
initIdentifierOffsets = IdentifierOffsets M.empty

totalOffset :: IdentifierOffsets -> Offset
totalOffset (IdentifierOffsets table) = 4 * M.size table

type ReplacingPseudoReg = State IdentifierOffsets

allocatePseudoReg :: AOperand -> ReplacingPseudoReg AOperand
allocatePseudoReg (APseudo identifier) = do
  table <- gets unIdentifierOffsets
  case table M.!? identifier of
    Just offset -> return (AStack offset)
    Nothing ->
      let offset = -4 - M.size table * 4
       in do
            modify (addToTable identifier offset)
            return (AStack offset)
allocatePseudoReg others = return others

replaceAPseudoReg :: AInstruction -> ReplacingPseudoReg AInstruction
replaceAPseudoReg instruction =
  case instruction of
    AMov src dst -> do
      src' <- allocatePseudoReg src
      dst' <- allocatePseudoReg dst
      return (AMov src' dst')
    AUnary operator operand -> do
      operand' <- allocatePseudoReg operand
      return $ AUnary operator operand'
    ABinary operator operand1 operand2 -> do
      operand1' <- allocatePseudoReg operand1
      operand2' <- allocatePseudoReg operand2
      return $ ABinary operator operand1' operand2'
    AIdiv operand -> do
      operand' <- allocatePseudoReg operand
      return $ AIdiv operand'
    others -> return others

replacePseudoRegs :: (Traversable t) => t AInstruction -> (Offset, t AInstruction)
replacePseudoRegs instructions =
  let (instructions', table) =
        runState (traverse replaceAPseudoReg instructions) initIdentifierOffsets
   in (totalOffset table, instructions')

convertProgramWithReplacePseudoRegs :: TKProgram -> (Offset, AProgram)
convertProgramWithReplacePseudoRegs tkP =
  let (AProgram (AFunction name instructions)) = convertProgram tkP
      (offset, instructions') = replacePseudoRegs instructions
   in (offset, AProgram (AFunction name instructions'))

-- * fixing up instructions

convertProgramWithFixedInstructions :: TKProgram -> AProgram
convertProgramWithFixedInstructions tkProgram =
  let (offset, AProgram (AFunction name instructions)) = convertProgramWithReplacePseudoRegs tkProgram
      instructions' = allocateStack offset : instructions
   in AProgram (AFunction name (fixInstructions instructions'))

-- ** insert allocateStack

allocateStack :: Offset -> AInstruction
allocateStack = AAllocateStack

-- ** invalid mov idiv add sub imul instructions

fixIns :: AInstruction -> [AInstruction]
fixIns (AMov (AStack src) (AStack dst)) =
  [ AMov (AStack src) (ARegister R10),
    AMov (ARegister R10) (AStack dst)
  ]
fixIns (AIdiv (AImm number)) =
  [ AMov (AImm number) (ARegister R10),
    AIdiv (ARegister R10)
  ]
fixIns (ABinary AAdd (AStack src) (AStack dst)) =
  [ AMov (AStack src) (ARegister R10),
    ABinary AAdd (ARegister R10) (AStack dst)
  ]
fixIns (ABinary ASub (AStack src) (AStack dst)) =
  [ AMov (AStack src) (ARegister R10),
    ABinary ASub (ARegister R10) (AStack dst)
  ]
fixIns (ABinary AMult src (AStack dst)) =
  [ AMov (AStack dst) (ARegister R11),
    ABinary AMult src (ARegister R11),
    AMov (ARegister R11) (AStack dst)
  ]
fixIns other = [other]

fixInstructions :: (Foldable t) => t AInstruction -> [AInstruction]
fixInstructions = concatMap fixIns
