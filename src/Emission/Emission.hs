module Emission.Emission
  ( constructProgram,
  )
where

import qualified AssemblyGen.AssemblyGen as AG

-- | linux stack line
linuxStackLine :: String
linuxStackLine = indent ++ ".section .note.GNU-stack,\"\",@progbits\n"

-- | 缩进
indent :: String
indent = "    "

constructProgram :: AG.Program -> String
constructProgram (AG.Program funcDef) =
  constructFunction funcDef
    ++ linuxStackLine

constructFunction :: AG.FuncDef -> String
constructFunction (AG.Function name instuctions) =
  indent
    ++ ".global "
    ++ AG.unIdentifier name
    ++ "\n"
    ++ AG.unIdentifier name
    ++ ":\n"
    ++ indent
    ++ "pushq %rbp\n"
    ++ indent
    ++ "movq %rsp, %rbp\n"
    ++ constructInstructions instuctions

constructInstructions :: AG.Instructions -> String
constructInstructions is =
  concat a
  where
    a = (\x -> indent ++ constructInstruction x) <$> is

constructInstruction :: AG.Instruction -> String
constructInstruction (AG.Mov src dst) =
  "movl" ++ " " ++ constructOperand src ++ ", " ++ constructOperand dst ++ "\n"
constructInstruction AG.Ret =
  "movq %rbp, %rsp\n"
    ++ indent
    ++ "popq %rbp\n"
    ++ indent
    ++ "ret\n"
constructInstruction (AG.Unary operator operand) =
  constructUnaryOperator operator ++ " " ++ constructOperand operand ++ "\n"
constructInstruction (AG.Binary operator src dst) =
  constructBinaryOperator operator ++ " " ++ constructOperand src ++ ", " ++ constructOperand dst ++ "\n"
constructInstruction (AG.Idiv operand) =
  "idivl " ++ constructOperand operand ++ "\n"
constructInstruction AG.Cdq =
  "cdq\n"
constructInstruction (AG.AllocateStack i) =
  "subq $" ++ show i ++ ", %rsp\n"
constructInstruction (AG.Cmp operand1 operand2) =
  "cmpl " ++ constructOperand operand1 ++ ", " ++ constructOperand operand2 ++ "\n"
constructInstruction (AG.Jmp label) =
  "jmp " ++ ".L" ++ AG.unIdentifier label ++ "\n"
constructInstruction (AG.JmpCC condCode label) =
  "j" ++ constructCondCode condCode ++ " " ++ ".L" ++ AG.unIdentifier label ++ "\n"
constructInstruction (AG.SetCC condCode operand) =
  "set" ++ constructCondCode condCode ++ " " ++ construct1BRegister operand ++ "\n"
constructInstruction (AG.Label label) =
  ".L" ++ AG.unIdentifier label ++ ":\n"

constructOperand :: AG.Operand -> String
constructOperand (AG.Imm imm) = "$" <> show imm
constructOperand (AG.Register AG.AX) = "%eax"
constructOperand (AG.Register AG.DX) = "%edx"
constructOperand (AG.Register AG.R10) = "%r10d"
constructOperand (AG.Register AG.R11) = "%r11d"
constructOperand (AG.Stack i) = show i <> "(%rbp)"
constructOperand (AG.Pseudo _) = error "unexpected operand during emission: Pseudo"

construct1BRegister :: AG.Operand -> String
construct1BRegister (AG.Imm imm) = "$" <> show imm
construct1BRegister (AG.Register AG.AX) = "%al"
construct1BRegister (AG.Register AG.DX) = "%dl"
construct1BRegister (AG.Register AG.R10) = "%r10b"
construct1BRegister (AG.Register AG.R11) = "%r11b"
construct1BRegister (AG.Stack i) = show i <> "(%rbp)"
construct1BRegister (AG.Pseudo _) = error "unexpected operand during emission: Pseudo"

constructUnaryOperator :: AG.UnaryOperator -> String
constructUnaryOperator AG.Neg = "negl"
constructUnaryOperator AG.Not = "notl"

constructBinaryOperator :: AG.BinaryOperator -> String
constructBinaryOperator AG.Add = "addl"
constructBinaryOperator AG.Sub = "subl"
constructBinaryOperator AG.Mult = "imull"

constructCondCode :: AG.CondCode -> String
constructCondCode AG.E = "e"
constructCondCode AG.NE = "ne"
constructCondCode AG.L = "l"
constructCondCode AG.LE = "le"
constructCondCode AG.G = "g"
constructCondCode AG.GE = "ge"