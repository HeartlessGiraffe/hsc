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
constructAInstruction (ACmp operand1 operand2) = 
  "cmpl " ++ constructAOperand operand1 ++ ", " ++ constructAOperand operand2 ++ "\n"
constructAInstruction (AJmp label) = 
  "jmp " ++ ".L" ++ unAIdentifier label ++ "\n"
constructAInstruction (AJmpCC condCode label) = 
  "j" ++ constructCondCode condCode ++ " " ++ ".L" ++ unAIdentifier label ++ "\n"
constructAInstruction (ASetCC condCode operand) = 
  "set" ++ constructCondCode condCode ++ " " ++ construct1BRegister operand ++ "\n"
constructAInstruction (ALabel label) = 
  ".L" ++ unAIdentifier label ++ ":\n"

constructAOperand :: AOperand -> String
constructAOperand (AImm imm) = "$" <> show imm
constructAOperand (ARegister AX) = "%eax"
constructAOperand (ARegister DX) = "%edx"
constructAOperand (ARegister R10) = "%r10d"
constructAOperand (ARegister R11) = "%r11d"
constructAOperand (AStack i) = show i <> "(%rbp)"
constructAOperand (APseudo _) = error "unexpected operand during emission: APseudo"

construct1BRegister :: AOperand -> String 
construct1BRegister (AImm imm) = "$" <> show imm
construct1BRegister (ARegister AX) = "%al"
construct1BRegister (ARegister DX) = "%dl"
construct1BRegister (ARegister R10) = "%r10b"
construct1BRegister (ARegister R11) = "%r11b"
construct1BRegister (AStack i) = show i <> "(%rbp)"
construct1BRegister (APseudo _) = error "unexpected operand during emission: APseudo"

constructUnaryOperator :: AUnaryOperator -> String
constructUnaryOperator ANeg = "negl"
constructUnaryOperator ANot = "notl"

constructBinaryOperator :: ABinaryOperator -> String
constructBinaryOperator AAdd = "addl"
constructBinaryOperator ASub = "subl"
constructBinaryOperator AMult = "imull"

constructCondCode :: ACondCode -> String 
constructCondCode E = "e"
constructCondCode NE = "ne"
constructCondCode L = "l"
constructCondCode LE = "le"
constructCondCode G = "g"
constructCondCode GE = "ge"