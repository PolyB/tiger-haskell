module Ast.PrettyPrinter (prettyPrint) where

--import Data.ByteString as BS
import Data.ByteString.Char8 as BS
import Data.List as L
import Ast
import Prelude ((++), (<$>), String, Maybe(Just, Nothing), show)

printFields:: Fields -> String
printFields f = L.intercalate "," ((\(x,y) -> BS.unpack x ++ ":" ++ BS.unpack y) <$> f)

class Printable a where
  prettyPrint :: a -> String


instance Printable LValue where
  prettyPrint (VarLV x) = BS.unpack x
  prettyPrint (FieldLV l x) = prettyPrint l ++ "." ++ BS.unpack x
  prettyPrint (AccessLV l exp) = prettyPrint l ++ "[" ++ prettyPrint exp ++ "]"

instance Printable Exp where
  prettyPrint (IfE c e Nothing) = "if " ++ prettyPrint c ++ " then " ++ prettyPrint e
  prettyPrint (IfE c e (Just o))= "if " ++ prettyPrint c ++ " then " ++ prettyPrint e ++ " else " ++ prettyPrint o
  prettyPrint (WhileE c e)      = "while " ++ prettyPrint c ++ " do " ++ prettyPrint e
  prettyPrint BreakE            = "break"
  prettyPrint (ForE v e1 e2 e3) = "for " ++ BS.unpack v ++ " := " ++ prettyPrint e1 ++ " to " ++ prettyPrint e2 ++ " do " ++ prettyPrint e3
  prettyPrint (LetE d e)        = "let " ++ L.intercalate "\n" (prettyPrint <$> d) ++ " in " ++ L.intercalate ";\n" (prettyPrint <$> e) ++ " end"
  prettyPrint (AssignE l e)     = prettyPrint l ++ " := " ++ prettyPrint e
  prettyPrint (SeqE e)          = "(" ++ L.intercalate ";\n" (prettyPrint <$> e) ++ ")"
  prettyPrint (OpE op e1 e2)    = prettyPrint e1 ++ prettyPrint op ++ prettyPrint e2
  prettyPrint (MethodE l s p)   = prettyPrint l ++ "." ++ BS.unpack s ++ "(" ++ L.intercalate "," (prettyPrint <$> p) ++ ")"
  prettyPrint (FunCallE v p)    = BS.unpack v ++ "(" ++ L.intercalate "," (prettyPrint <$> p) ++ ")"
  prettyPrint (LValueE l)       = prettyPrint l
  prettyPrint (ArrayE b e1 e2)  = BS.unpack b ++ "[" ++ prettyPrint e1 ++ "] of" ++ prettyPrint e2
  prettyPrint (RecordE b r)     = BS.unpack b ++ "{" ++ L.intercalate "," ((\(x,y) -> BS.unpack x ++ "=" ++ prettyPrint y)<$>r) ++ "}"
  prettyPrint NilE              = "nil"
  prettyPrint (IntegerE i)      = show i
  prettyPrint (StringE s)       = show s

instance Printable Op where
  prettyPrint  MultOp         = "*"
  prettyPrint  DivOp          = "/"
  prettyPrint  PlusOp         = "+"
  prettyPrint  MinusOp        = "-"
  prettyPrint  EqualOp        = "="
  prettyPrint  DiffOp         = "<>"
  prettyPrint  InferiorOp     = "<"
  prettyPrint  SuperiorOp     = ">"
  prettyPrint  InferiorEqOp   = "<="
  prettyPrint  SuperiorEqOp   = ">="
  prettyPrint  AndOp          = "&"
  prettyPrint  OrOp           = "|"

instance Printable Type where
  prettyPrint (BaseT b)       = BS.unpack b
  prettyPrint (FieldsT f)     = "{" ++ printFields f ++ "}"
  prettyPrint (ArrayT t)      = "array of " ++ BS.unpack t

instance Printable Dec where
  prettyPrint (AliasD b t)            = "type " ++ BS.unpack b ++ "=" ++ prettyPrint t
  prettyPrint (VarD v Nothing e)      = "var " ++ BS.unpack v ++ ":=" ++ prettyPrint e
  prettyPrint (VarD v (Just t) e)     = "var " ++ BS.unpack v ++ ":" ++ BS.unpack t++ ":=" ++ prettyPrint e
  prettyPrint (FunD v f Nothing e)    = "function " ++ BS.unpack v ++ "(" ++ printFields f ++ ")" ++ "=" ++ prettyPrint e
  prettyPrint (FunD v f (Just t) e)   = "function " ++ BS.unpack v ++ "(" ++ printFields f ++ ") :" ++ BS.unpack t ++ "=" ++ prettyPrint e
  prettyPrint (PrimD v f Nothing)     = "primitive " ++ BS.unpack v ++ "(" ++ printFields f ++ ")"
  prettyPrint (PrimD v f (Just t))    = "primitive " ++ BS.unpack v ++ "(" ++ printFields f ++ ") :" ++ BS.unpack t
  prettyPrint (ImportD s)             = "import" ++ show s
