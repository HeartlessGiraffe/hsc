module Emission.Emission
  ( constructAProgram,
  )
where

import AssemblyGen.AssemblyGen

-- | linux stack line
linuxStackLine :: String
linuxStackLine = indent ++ ".section .note.GNU-stack,\"\",@progbits\n"

-- | 缩进
indent :: String
indent = "    "

constructAProgram :: AProgram -> String
constructAProgram (AProgram funcDef) =
  constructAFunction funcDef
    ++ linuxStackLine

constructAFunction :: AFuncDef -> String
constructAFunction (AFunction name instuctions) =
  indent
    ++ ".global "
    ++ unAIdentifier name
    ++ "\n"
    ++ unAIdentifier name
    ++ ":\n"
    ++ constructAInstructions instuctions

constructAInstructions :: [AInstruction] -> String
constructAInstructions is =
  concat a
  where
    a = (\x -> indent ++ constructAInstruction x) <$> is

constructAInstruction :: AInstruction -> String
constructAInstruction (AMov src dst) =
  "movl" ++ " " ++ constructAOperand src ++ ", " ++ constructAOperand dst ++ "\n"
constructAInstruction ARet =
  "ret\n"

constructAOperand :: AOperand -> String
constructAOperand (AImm imm) = "$" ++ show imm
constructAOperand ARegister = "%eax"