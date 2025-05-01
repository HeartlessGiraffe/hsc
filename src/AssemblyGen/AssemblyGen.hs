{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module AssemblyGen.AssemblyGen
  ( -- * Assembly represented By ADT
    Program (..),
    FuncDef (..),
    Identifier (..),
    Instruction (..),
    UnaryOperator (..),
    BinaryOperator (..),
    Operand (..),
    CondCode (..),
    Reg (..),

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
import qualified TACKY.TACKY as TACKY
import qualified Text.PrettyPrint as PP
import Utils (Pretty (..))

-- * Assembly represented By ADT

-- ** AST Definition

-- program = Program(function_definition)
-- function_definition = Function(identifier name, instruction* instructions)
-- instruction = Mov(operand src, operand dst)
--             | Unary(unary_operator, operand)
--             | Binary(binary_operator, operand, operand)
--             | Cmp(operand, operand)
--             | Idiv(operand)
--             | Cdq
--             | Jmp(identifier)
--             | JmpCC(cond_code, identifier)
--             | SetCC(cond_code, operand)
--             | Label(identifier)
--             | AllocateStack(int)
--             | Ret
-- unary_operator = Neg | Not
-- binary_operator = Add | Sub | Mult
-- operand = Imm(int) | Reg(reg) | Pseudo(identifier) | Stack(int)
-- cond_code = E | NE | G | GE | L | LE
-- reg = AX | DX | R10 | R11

data Program = Program FuncDef
  deriving (Show, Eq)

data FuncDef = Function
  { _aFuncName :: Identifier,
    _aFuncInstructions :: [Instruction]
  }
  deriving (Show, Eq)

newtype Identifier = Identifier
  { unIdentifier :: String
  }
  deriving (Show, Eq, Ord)

data Instruction
  = Mov
      { _movSrc :: Operand,
        _movDst :: Operand
      }
  | Unary UnaryOperator Operand
  | Binary BinaryOperator Operand Operand
  | Cmp Operand Operand
  | Idiv Operand
  | Cdq
  | Jmp Identifier
  | JmpCC CondCode Identifier
  | SetCC CondCode Operand
  | Label Identifier
  | AllocateStack Int
  | Ret
  deriving (Show, Eq)

data UnaryOperator = Neg | Not
  deriving (Show, Eq)

data BinaryOperator = Add | Sub | Mult
  deriving (Show, Eq)

data Operand = Imm Int | Register Reg | Pseudo Identifier | Stack Int
  deriving (Show, Eq)

data CondCode = E | NE | G | GE | L | LE
  deriving (Show, Eq)

data Reg = AX | DX | R10 | R11
  deriving (Show, Eq)

-- **  Pretty Printing

instance Pretty Program where
  pretty (Program funcDef) =
    PP.text "Program {" PP.$$ PP.nest 2 (pretty funcDef) PP.$$ PP.text "}"

instance Pretty FuncDef where
  pretty (Function name instructions) =
    PP.text "Function {" PP.$$ PP.nest 2 (pretty name) PP.$$ PP.nest 2 (PP.vcat (map pretty instructions)) PP.$$ PP.text "}"

instance Pretty Identifier where
  pretty (Identifier name) = PP.text name

instance Pretty Instruction where
  pretty (Mov src dst) =
    PP.text "Mov" PP.<+> (pretty src <> PP.text ",") PP.<+> pretty dst
  pretty (Unary operator oprand) =
    PP.text "Unary" PP.<+> (pretty operator <> PP.text ",") PP.<+> pretty oprand
  pretty (Binary operator oprand1 oprand2) =
    PP.text "Binary" PP.<+> (pretty operator <> PP.text ",") PP.<+> (pretty oprand1 <> PP.text ",") PP.<+> pretty oprand2
  pretty (Cmp oprand1 oprand2) =
    PP.text "Cmp" PP.<+> (pretty oprand1 <> PP.text ",") PP.<+> pretty oprand2
  pretty (Idiv operator) =
    PP.text "Idiv" PP.<+> pretty operator
  pretty Cdq =
    PP.text "Cdq"
  pretty (Jmp i) =
    PP.text "Jmp" PP.<+> pretty i
  pretty (JmpCC cc i) =
    PP.text "JmpCC" PP.<+> (pretty cc <> PP.text ",") PP.<+> pretty i
  pretty (SetCC cc oprand) =
    PP.text "JmpCC" PP.<+> (pretty cc <> PP.text ",") PP.<+> pretty oprand
  pretty (Label i) =
    PP.text "Label" PP.<+> pretty i
  pretty (AllocateStack i) =
    PP.text "AllocateStack" PP.<+> PP.int i
  pretty Ret = PP.text "Ret"

instance Pretty Operand where
  pretty (Imm i) = PP.text "Imm" PP.<+> PP.int i
  pretty (Register reg) = PP.text "Register" PP.<+> pretty reg
  pretty (Pseudo i) = PP.text "Pseudo" PP.<+> pretty i
  pretty (Stack i) = PP.text "Stack" PP.<+> PP.int i

instance Pretty UnaryOperator where
  pretty Neg = PP.text "Neg"
  pretty Not = PP.text "Not"

instance Pretty BinaryOperator where
  pretty Add = PP.text "Add"
  pretty Sub = PP.text "Sub"
  pretty Mult = PP.text "Mult"

instance Pretty CondCode where
  pretty E = PP.text "E"
  pretty NE = PP.text "NE"
  pretty G = PP.text "G"
  pretty GE = PP.text "GE"
  pretty L = PP.text "L"
  pretty LE = PP.text "LE"

instance Pretty Reg where
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
convertProgram :: TACKY.Program -> Program
convertProgram (TACKY.Program funcDef) = Program (convertFunction funcDef)

convertFunction :: TACKY.FuncDef -> FuncDef
convertFunction (TACKY.Function name instuctions) =
  Function (convertIdentifier name) (concatMap convertInstruction instuctions)

convertInstruction :: TACKY.Instruction -> [Instruction]
convertInstruction (TACKY.Return val) =
  [ Mov (convertVal val) (Register AX),
    Ret
  ]
convertInstruction (TACKY.Unary TACKY.Not src dst) =
  [ Cmp (Imm 0) (convertVal src),
    Mov (Imm 0) (convertVal dst),
    SetCC E (convertVal dst)
  ]
convertInstruction (TACKY.Unary uOperator src dst) =
  [ Mov (convertVal src) (convertVal dst),
    Unary (convertUnaryOperator uOperator) (convertVal dst)
  ]
convertInstruction (TACKY.Binary TACKY.Divide src1 src2 dst) =
  [ Mov (convertVal src1) (Register AX),
    Cdq,
    Idiv (convertVal src2),
    Mov (Register AX) (convertVal dst)
  ]
convertInstruction (TACKY.Binary TACKY.Remainder src1 src2 dst) =
  [ Mov (convertVal src1) (Register AX),
    Cdq,
    Idiv (convertVal src2),
    Mov (Register DX) (convertVal dst)
  ]
convertInstruction (TACKY.Binary bOperator src1 src2 dst) =
  if TACKY.isRelationalOperator bOperator
    then
      [ Cmp (convertVal src2) (convertVal src1),
        Mov (Imm 0) (convertVal dst),
        SetCC (convertComparison bOperator) (convertVal dst)
      ]
    else
      [ Mov (convertVal src1) (convertVal dst),
        Binary (convertBinaryOperator bOperator) (convertVal src2) (convertVal dst)
      ]
convertInstruction (TACKY.JumpIfZero val target) =
  [ Cmp (Imm 0) (convertVal val),
    JmpCC E (convertIdentifier target)
  ]
convertInstruction (TACKY.JumpIfNotZero val target) =
  [ Cmp (Imm 0) (convertVal val),
    JmpCC NE (convertIdentifier target)
  ]
convertInstruction (TACKY.Jump target) =
  [ Jmp (convertIdentifier target)
  ]
convertInstruction (TACKY.Label name) =
  [ Label (convertIdentifier name)
  ]
convertInstruction (TACKY.Copy src dst) =
  [ Mov (convertVal src) (convertVal dst)
  ]

convertIdentifier :: TACKY.Identifier -> Identifier
convertIdentifier (TACKY.Identifier name) = Identifier name

convertUnaryOperator :: TACKY.UnaryOperator -> UnaryOperator
convertUnaryOperator TACKY.Complement = Not
convertUnaryOperator TACKY.Negate = Neg
convertUnaryOperator TACKY.Not = Not

convertComparison :: TACKY.BinaryOperator -> CondCode
convertComparison TACKY.Equal = E
convertComparison TACKY.NotEqual = NE
convertComparison TACKY.LessThan = L
convertComparison TACKY.LessOrEqual = LE
convertComparison TACKY.GreaterThan = G
convertComparison TACKY.GreaterOrEqual = GE
convertComparison op = error $ "convertComparison: " ++ show op ++ " shouldn't be convert!"

convertBinaryOperator :: TACKY.BinaryOperator -> BinaryOperator
convertBinaryOperator TACKY.Add = Add
convertBinaryOperator TACKY.Subtract = Sub
convertBinaryOperator TACKY.Multiply = Mult
convertBinaryOperator op = error $ "convertBinaryOperator: " ++ show op ++ " shouldn't be convert!"

convertVal :: TACKY.Val -> Operand
convertVal (TACKY.Constant i) = Imm i
convertVal (TACKY.Var i) = Pseudo (convertIdentifier i)

-- * replacing pseudoregisters

type Offset = Int

newtype IdentifierOffsets = IdentifierOffsets
  { unIdentifierOffsets :: M.Map Identifier Offset
  }

addToTable :: Identifier -> Offset -> IdentifierOffsets -> IdentifierOffsets
addToTable identifier offset (IdentifierOffsets table) =
  IdentifierOffsets $ M.insert identifier offset table

initIdentifierOffsets :: IdentifierOffsets
initIdentifierOffsets = IdentifierOffsets M.empty

totalOffset :: IdentifierOffsets -> Offset
totalOffset (IdentifierOffsets table) = 4 * M.size table

type ReplacingPseudoReg = State IdentifierOffsets

allocatePseudoReg :: Operand -> ReplacingPseudoReg Operand
allocatePseudoReg (Pseudo identifier) = do
  table <- gets unIdentifierOffsets
  case table M.!? identifier of
    Just offset -> return (Stack offset)
    Nothing ->
      let offset = -4 - M.size table * 4
       in do
            modify (addToTable identifier offset)
            return (Stack offset)
allocatePseudoReg others = return others

replacePseudoReg :: Instruction -> ReplacingPseudoReg Instruction
replacePseudoReg instruction =
  case instruction of
    Mov src dst -> do
      src' <- allocatePseudoReg src
      dst' <- allocatePseudoReg dst
      return (Mov src' dst')
    Unary operator operand -> do
      operand' <- allocatePseudoReg operand
      return $ Unary operator operand'
    Binary operator operand1 operand2 -> do
      operand1' <- allocatePseudoReg operand1
      operand2' <- allocatePseudoReg operand2
      return $ Binary operator operand1' operand2'
    Idiv operand -> do
      operand' <- allocatePseudoReg operand
      return $ Idiv operand'
    Cmp operand1 operand2 -> do
      operand1' <- allocatePseudoReg operand1
      operand2' <- allocatePseudoReg operand2
      return $ Cmp operand1' operand2'
    SetCC opecode operand -> do
      operand' <- allocatePseudoReg operand
      return $ SetCC opecode operand'
    others -> return others

replacePseudoRegs :: (Traversable t) => t Instruction -> (Offset, t Instruction)
replacePseudoRegs instructions =
  let (instructions', table) =
        runState (traverse replacePseudoReg instructions) initIdentifierOffsets
   in (totalOffset table, instructions')

convertProgramWithReplacePseudoRegs :: TACKY.Program -> (Offset, Program)
convertProgramWithReplacePseudoRegs tkP =
  let (Program (Function name instructions)) = convertProgram tkP
      (offset, instructions') = replacePseudoRegs instructions
   in (offset, Program (Function name instructions'))

-- * fixing up instructions

convertProgramWithFixedInstructions :: TACKY.Program -> Program
convertProgramWithFixedInstructions tkProgram =
  let (offset, Program (Function name instructions)) = convertProgramWithReplacePseudoRegs tkProgram
      instructions' = allocateStack offset : instructions
   in Program (Function name (fixInstructions instructions'))

-- ** insert allocateStack

allocateStack :: Offset -> Instruction
allocateStack = AllocateStack

-- ** invalid mov idiv add sub imul instructions

fixIns :: Instruction -> [Instruction]
fixIns (Mov (Stack src) (Stack dst)) =
  [ Mov (Stack src) (Register R10),
    Mov (Register R10) (Stack dst)
  ]
fixIns (Idiv (Imm number)) =
  [ Mov (Imm number) (Register R10),
    Idiv (Register R10)
  ]
fixIns (Binary Add (Stack src) (Stack dst)) =
  [ Mov (Stack src) (Register R10),
    Binary Add (Register R10) (Stack dst)
  ]
fixIns (Binary Sub (Stack src) (Stack dst)) =
  [ Mov (Stack src) (Register R10),
    Binary Sub (Register R10) (Stack dst)
  ]
fixIns (Binary Mult src (Stack dst)) =
  [ Mov (Stack dst) (Register R11),
    Binary Mult src (Register R11),
    Mov (Register R11) (Stack dst)
  ]
fixIns (Cmp (Stack src) (Stack dst)) =
  [ Mov (Stack src) (Register R10),
    Cmp (Register R10) (Stack dst)
  ]
fixIns (Cmp src (Imm i)) =
  [ Mov (Imm i) (Register R11),
    Cmp src (Register R11)
  ]
fixIns other = [other]

fixInstructions :: (Foldable t) => t Instruction -> [Instruction]
fixInstructions = concatMap fixIns
