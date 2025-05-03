module Utils.Pretty
  ( Pretty (..),
    prettyPrint,
  )
where

import qualified Text.PrettyPrint as PP
import qualified Parser.Parser as Parser 
import qualified TACKY.TACKY as TACKY
import qualified AssemblyGen.AssemblyGen as AG
import Data.Foldable (Foldable(..))

-- | A typeclass for pretty printing
class Pretty a where
  -- | Pretty print a value
  pretty :: a -> PP.Doc

-- | Pretty print as a string
prettyPrint :: (Pretty a) => a -> String
prettyPrint = PP.render . pretty

-- C AST

instance Pretty Parser.Identifier where
  pretty (Parser.Identifier name) = PP.text name

instance Pretty Parser.Program where
  pretty (Parser.Program funcDef) =
    PP.text "Program {" PP.$$ PP.nest 2 (pretty funcDef) PP.$$ PP.text "}"

instance Pretty Parser.BlockItem where 
  pretty (Parser.S s) = 
    PP.text "S (" <> pretty s <> PP.text ")"
  pretty (Parser.D d) = 
    PP.text "D (" <> pretty d <> PP.text ")"

instance Pretty Parser.Declaration where 
  pretty (Parser.Declaration i mE) = 
    case mE of 
      Nothing -> pretty i
      Just e -> pretty i <> PP.text ", " <> pretty e

instance Pretty Parser.FuncDef where
  pretty (Parser.Function name body) =
    PP.text "Function (" <> pretty name <> PP.text ") {" PP.$$ PP.nest 2 (PP.sep . toList $ pretty <$> body) PP.$$ PP.text "}"

instance Pretty Parser.Statement where
  pretty (Parser.Return expr) =
    PP.text "Return (" <> pretty expr <> PP.text ")"
  pretty (Parser.Expression expr) =
    PP.text "Expression (" <> pretty expr <> PP.text ")"
  pretty Parser.Null =
    PP.text "Null"
  pretty (Parser.If cond condthen mcondelse) = 
    PP.text "If (" <> pretty cond <> PP.text ")" <> PP.text "then(" <> pretty condthen <> PP.text ")" 
    <> (case mcondelse of 
          Just condelse -> PP.text "else (" <> pretty condelse <> PP.text ")"
          Nothing -> PP.text ""
      )

instance Pretty Parser.Exp where
  pretty (Parser.Constant value) =
    PP.text "Constant(" <> PP.int value <> PP.text ")"
  pretty (Parser.Var i) =
    PP.text "Var(" <> pretty i <> PP.text ")"
  pretty (Parser.Unary uOp e) =
    PP.text "Unary(" <> pretty uOp <> PP.text ", " <> pretty e <> PP.text ")"
  pretty (Parser.Binary bOp e1 e2) =
    if Parser.isConstant e1 && Parser.isConstant e2
      then
        PP.text "Binary(" <> pretty bOp <> PP.text ", " <> pretty e1 <> PP.text ", " <> pretty e2 <> PP.text ")"
      else PP.text "Binary(" PP.$$ PP.nest 2 (pretty bOp) <> PP.text ", " PP.$$ PP.nest 2 (pretty e1) <> PP.text ", " PP.$$ PP.nest 2 (pretty e2) PP.$$ PP.text ")"
  pretty (Parser.Assignment e1 e2) =
    PP.text "Assignment(" <> pretty e1 <> PP.text ", " <> pretty e2 <> PP.text ")"
  pretty (Parser.Conditional condE e1 e2) = 
    PP.text "Conditional(" <> pretty condE <> PP.text ", " <> pretty e1 <> PP.text ", " <> pretty e2 <> PP.text ")"

instance Pretty Parser.UnaryOperator where
  pretty Parser.Complement = PP.text "Complement"
  pretty Parser.Negate = PP.text "Negate"
  pretty Parser.Not = PP.text "Not"

instance Pretty Parser.BinaryOperator where
  pretty Parser.Add = PP.text "Add"
  pretty Parser.Subtract = PP.text "Subtract"
  pretty Parser.Multiply = PP.text "Multiply"
  pretty Parser.Divide = PP.text "Divide"
  pretty Parser.Remainder = PP.text "Remainder"
  pretty Parser.And = PP.text "And"
  pretty Parser.Or = PP.text "Or"
  pretty Parser.Equal = PP.text "Equal"
  pretty Parser.NotEqual = PP.text "NotEqual"
  pretty Parser.LessThan = PP.text "LessThan"
  pretty Parser.GreaterThan = PP.text "GreaterThan"
  pretty Parser.LessOrEqual = PP.text "LessOrEqual"
  pretty Parser.GreaterOrEqual = PP.text "GreaterOrEqual"

-- TACKY AST

instance Pretty TACKY.Identifier where
  pretty (TACKY.Identifier name) = PP.text name

instance Pretty TACKY.Program where
  pretty (TACKY.Program funcDef) =
    PP.text "Program {" PP.$$ PP.nest 2 (pretty funcDef) PP.$$ PP.text "}"

instance Pretty TACKY.FuncDef where
  pretty (TACKY.Function name body) =
    PP.text "Function (" <> pretty name <> PP.text ") {" PP.$$ PP.nest 2 (PP.sep $ toList (pretty <$> body)) PP.$$ PP.text "}"

instance Pretty TACKY.Instruction where
  pretty (TACKY.Return expr) =
    PP.text "Return(" <> pretty expr <> PP.text ")"
  pretty (TACKY.Unary op src dst) =
    PP.text "Unary(" <> pretty op <> PP.text ", " <> pretty src <> PP.text ", " <> pretty dst <> PP.text ")"
  pretty (TACKY.Binary op src1 src2 dst) =
    if TACKY.isConstant src1 && TACKY.isConstant src2 && TACKY.isConstant dst
      then
        PP.text "Binary(" <> pretty op <> PP.text ", " <> pretty src1 <> PP.text ", " <> pretty src2 <> PP.text ", " <> pretty dst <> PP.text ")"
      else PP.text "Binary(" PP.$$ PP.nest 2 (pretty op) <> PP.text ", " PP.$$ PP.nest 2 (pretty src1) PP.$$ PP.nest 2 (pretty src2) <> PP.text ", " PP.$$ PP.nest 2 (pretty dst) PP.$$ PP.text ")"
  pretty (TACKY.Copy src dst) =
    PP.text "Copy(" <> pretty src <> PP.text ", " <> pretty dst <> PP.text ")"
  pretty (TACKY.Jump target) =
    PP.text "Jump(" <> pretty target <> PP.text ")"
  pretty (TACKY.JumpIfZero cond target) =
    PP.text "JumpIfZero(" <> pretty cond <> PP.text ", " <> pretty target <> PP.text ")"
  pretty (TACKY.JumpIfNotZero cond target) =
    PP.text "JumpIfNotZero(" <> pretty cond <> PP.text ", " <> pretty target <> PP.text ")"
  pretty (TACKY.Label label) =
    PP.text "Label(" <> pretty label <> PP.text ")"

instance Pretty TACKY.Val where
  pretty (TACKY.Constant c) = PP.text "Constant " <> PP.text (show c)
  pretty (TACKY.Var i) = PP.text "Var " <> pretty i

instance Pretty TACKY.UnaryOperator where
  pretty TACKY.Complement = PP.text "Complement"
  pretty TACKY.Negate = PP.text "Negate"
  pretty TACKY.Not = PP.text "Not"

instance Pretty TACKY.BinaryOperator where
  pretty TACKY.Add = PP.text "Add"
  pretty TACKY.Subtract = PP.text "Subtract"
  pretty TACKY.Multiply = PP.text "Multiply"
  pretty TACKY.Divide = PP.text "Divide"
  pretty TACKY.Remainder = PP.text "Remainder"
  pretty TACKY.Equal = PP.text "Equal"
  pretty TACKY.NotEqual = PP.text "NotEqual"
  pretty TACKY.GreaterThan = PP.text "GreaterThan"
  pretty TACKY.GreaterOrEqual = PP.text "GreaterOrEqual"
  pretty TACKY.LessThan = PP.text "LessThan"
  pretty TACKY.LessOrEqual = PP.text "LessOrEqual"

-- Assembly AST


instance Pretty AG.Program where
  pretty (AG.Program funcDef) =
    PP.text "Program {" PP.$$ PP.nest 2 (pretty funcDef) PP.$$ PP.text "}"

instance Pretty AG.FuncDef where
  pretty (AG.Function name instructions) =
    PP.text "Function {" PP.$$ PP.nest 2 (pretty name) PP.$$ PP.nest 2 (PP.vcat (toList (fmap pretty instructions))) PP.$$ PP.text "}"

instance Pretty AG.Identifier where
  pretty (AG.Identifier name) = PP.text name

instance Pretty AG.Instruction where
  pretty (AG.Mov src dst) =
    PP.text "Mov" PP.<+> (pretty src <> PP.text ",") PP.<+> pretty dst
  pretty (AG.Unary operator oprand) =
    PP.text "Unary" PP.<+> (pretty operator <> PP.text ",") PP.<+> pretty oprand
  pretty (AG.Binary operator oprand1 oprand2) =
    PP.text "Binary" PP.<+> (pretty operator <> PP.text ",") PP.<+> (pretty oprand1 <> PP.text ",") PP.<+> pretty oprand2
  pretty (AG.Cmp oprand1 oprand2) =
    PP.text "Cmp" PP.<+> (pretty oprand1 <> PP.text ",") PP.<+> pretty oprand2
  pretty (AG.Idiv operator) =
    PP.text "Idiv" PP.<+> pretty operator
  pretty AG.Cdq =
    PP.text "Cdq"
  pretty (AG.Jmp i) =
    PP.text "Jmp" PP.<+> pretty i
  pretty (AG.JmpCC cc i) =
    PP.text "JmpCC" PP.<+> (pretty cc <> PP.text ",") PP.<+> pretty i
  pretty (AG.SetCC cc oprand) =
    PP.text "JmpCC" PP.<+> (pretty cc <> PP.text ",") PP.<+> pretty oprand
  pretty (AG.Label i) =
    PP.text "Label" PP.<+> pretty i
  pretty (AG.AllocateStack i) =
    PP.text "AllocateStack" PP.<+> PP.int i
  pretty AG.Ret = PP.text "Ret"

instance Pretty AG.Operand where
  pretty (AG.Imm i) = PP.text "Imm" PP.<+> PP.int i
  pretty (AG.Register reg) = PP.text "Register" PP.<+> pretty reg
  pretty (AG.Pseudo i) = PP.text "Pseudo" PP.<+> pretty i
  pretty (AG.Stack i) = PP.text "Stack" PP.<+> PP.int i

instance Pretty AG.UnaryOperator where
  pretty AG.Neg = PP.text "Neg"
  pretty AG.Not = PP.text "Not"

instance Pretty AG.BinaryOperator where
  pretty AG.Add = PP.text "Add"
  pretty AG.Sub = PP.text "Sub"
  pretty AG.Mult = PP.text "Mult"

instance Pretty AG.CondCode where
  pretty AG.E = PP.text "E"
  pretty AG.NE = PP.text "NE"
  pretty AG.G = PP.text "G"
  pretty AG.GE = PP.text "GE"
  pretty AG.L = PP.text "L"
  pretty AG.LE = PP.text "LE"

instance Pretty AG.Reg where
  pretty AG.AX = PP.text "AX"
  pretty AG.DX = PP.text "DX"
  pretty AG.R10 = PP.text "R10"
  pretty AG.R11 = PP.text "R11"