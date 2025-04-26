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
    ++ indent
    ++ "pushq %rbp\n"
    ++ indent
    ++ "movq %rsp, %rbp\n"
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
  "movq %rbp, %rsp\n"
    ++ indent
    ++ "popq %rbp\n"
    ++ indent
    ++ "ret\n"
constructAInstruction (AUnary operator operand) =
  constructUnaryOperator operator ++ " " ++ constructAOperand operand ++ "\n"
constructAInstruction (ABinary operator src dst) =
  constructBinaryOperator operator ++ " " ++ constructAOperand src ++ ", " ++ constructAOperand dst ++ "\n"
constructAInstruction (AIdiv operand) =
  "idivl " ++ constructAOperand operand ++ "\n"
constructAInstruction ACdq =
  "cdq\n"
constructAInstruction (AAllocateStack i) =
  "subq $" ++ show i ++ ", %rsp\n"

constructAOperand :: AOperand -> String
constructAOperand (AImm imm) = "$" <> show imm
constructAOperand (ARegister AX) = "%eax"
constructAOperand (ARegister DX) = "%edx"
constructAOperand (ARegister R10) = "%r10d"
constructAOperand (ARegister R11) = "%r11d"
constructAOperand (AStack i) = show i <> "(%rbp)"
constructAOperand (APseudo _) = error "unexpected operand during emission: APseudo"

constructUnaryOperator :: AUnaryOperator -> String
constructUnaryOperator ANeg = "negl"
constructUnaryOperator ANot = "notl"

constructBinaryOperator :: ABinaryOperator -> String
constructBinaryOperator AAdd = "addl"
constructBinaryOperator ASub = "subl"
constructBinaryOperator AMult = "imull"
