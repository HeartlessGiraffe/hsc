{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module AssemblyGen.AssemblyGen
  ( -- * Assembly represented By ADT
    AProgram (..),
    AFuncDef (..),
    AIdentifier (..),
    AInstruction (..),
    AUnaryOperator(..),
    AOperand (..),
    AReg(..),

    -- * Converting
    convertProgram,
    convertProgramWithReplacePseudoRegs,
    convertProgramWithFixedInstructions
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

-- | Unary(unary_operator, operand)
-- | AllocateStack(int)
-- | Ret
-- unary_operator = Neg | Not
-- operand = Imm(int) | Reg(reg) | Pseudo(identifier) | Stack(int)
-- reg = AX | R10
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
  | AAllocateStack Int
  | ARet
  deriving (Show, Eq)

data AUnaryOperator = ANeg | ANot
  deriving (Show, Eq)

data AOperand = AImm Int | ARegister AReg | APseudo AIdentifier | AStack Int
  deriving (Show, Eq)

data AReg = AX | R10
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

instance Pretty AReg where
  pretty AX = PP.text "AX"
  pretty R10 = PP.text "R10"

-- * Converting

-- from:
-- program = Program(function_definition)
-- function_definition = Function(identifier, 1 instruction* body)
-- instruction = Return(val) | Unary(unary_operator, val src, val dst)
-- val = Constant(int) | Var(identifier)
-- unary_operator = Complement | Negate
--
-- to:
-- program = Program(function_definition)
-- function_definition = Function(identifier name, instruction* instructions)
-- instruction = Mov(operand src, operand dst)

-- | Unary(unary_operator, operand)
-- | AllocateStack(int)
-- | Ret
-- unary_operator = Neg | Not
-- operand = Imm(int) | Reg(reg) | Pseudo(identifier) | Stack(int)
-- reg = AX | R10
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

convertIdentifier :: TKIdentifier -> AIdentifier
convertIdentifier (TKIdentifier name) = AIdentifier name

convertUnaryOperator :: TKUnaryOperator -> AUnaryOperator
convertUnaryOperator TKComplement = ANot
convertUnaryOperator TKNegate = ANeg

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
      let offset = - 4 - M.size table * 4
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
    others -> return others

replacePseudoRegs :: (Traversable t) => t AInstruction -> (Offset, t AInstruction)
replacePseudoRegs instructions =
  let (instructions', table) =
        runState (traverse replaceAPseudoReg instructions) initIdentifierOffsets
   in (totalOffset table, instructions')

convertProgramWithReplacePseudoRegs :: TKProgram -> (Offset, AProgram)
convertProgramWithReplacePseudoRegs tkP = let 
  (AProgram (AFunction name instructions)) = convertProgram tkP
  (offset, instructions') = replacePseudoRegs instructions
  in (offset, AProgram (AFunction name instructions')) 

-- * fixing up instructions

convertProgramWithFixedInstructions :: TKProgram -> AProgram
convertProgramWithFixedInstructions tkProgram = let 
  (offset, AProgram (AFunction name instructions)) = convertProgramWithReplacePseudoRegs tkProgram
  instructions' = allocateStack offset : instructions
  in AProgram (AFunction name (fixInstructions instructions'))

-- ** insert allocateStack

allocateStack :: Offset -> AInstruction
allocateStack = AAllocateStack

-- ** invalid mov instructions

fixMov :: AInstruction -> [AInstruction]
fixMov (AMov (AStack src) (AStack dst)) = [
  AMov (AStack src) (ARegister R10),
  AMov (ARegister R10) (AStack dst)
  ]
fixMov other = [other]

fixInstructions :: (Foldable t) => t AInstruction -> [AInstruction]
fixInstructions = concatMap fixMov

