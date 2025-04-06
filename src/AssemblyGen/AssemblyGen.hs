{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module AssemblyGen.AssemblyGen
  ( -- * Assembly represented By ADT
    AProgram (..),
    AFuncDef (..),
    AIdentifier (..),
    AInstruction (..),
    AOperand (..),

    -- * Converting
    convertProgram,
    convertFunction,
    convertReturn,
    convertConstant,
    convertIdentifier,
  )
where

import Parser.Parser
import qualified Text.PrettyPrint as PP
import Utils (Pretty (..))

-- * Assembly represented By ADT

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
  deriving (Show, Eq)

data AInstruction
  = AMov
      { _aMovSrc :: AOperand,
        _aMovDst :: AOperand
      }
  | ARet
  deriving (Show, Eq)

data AOperand = AImm Int | ARegister
  deriving (Show, Eq)

-- * Pretty Printing

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
    PP.text "AMov" PP.<+> pretty src PP.<+> pretty dst
  pretty ARet = PP.text "ARet"

instance Pretty AOperand where
  pretty (AImm i) = PP.text "AImm" PP.<+> PP.int i
  pretty ARegister = PP.text "ARegister"

-- * Converting

convertProgram :: Program -> AProgram
convertProgram (Program funcDef) = AProgram (convertFunction funcDef)

convertFunction :: FuncDef -> AFuncDef
convertFunction (Function name body) =
  AFunction (convertIdentifier name) (convertReturn body)

convertReturn :: Statement -> [AInstruction]
convertReturn (Return expr) =
  [ AMov (convertConstant expr) ARegister,
    ARet
  ]

convertConstant :: Exp -> AOperand
convertConstant (Constant i) = AImm i

convertIdentifier :: Identifier -> AIdentifier
convertIdentifier (Identifier name) = AIdentifier name