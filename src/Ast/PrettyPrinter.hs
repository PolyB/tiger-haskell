{-# LANGUAGE FlexibleInstances #-}
module Ast.PrettyPrinter (prettyPrint) where
--import Data.ByteString as BS
import Ast
import Data.ByteString.Char8 as BS
import Data.Functor.Foldable
import Data.List as L
import Prelude ((++), (<$>), String, Maybe(Just, Nothing), show, ($))
import Data.Functor (Functor)

prettyPrint::(Functor a, Printable (a String)) => a Exp -> String
prettyPrint f = pp $ cata pp <$> f

class Printable r where
  pp :: r -> String

instance Printable Fields where
  pp f = L.intercalate "," ((\(x,y) -> BS.unpack x ++ ":" ++ BS.unpack y) <$> f)



instance Printable (LValueF String) where
  pp (VarLV x) = BS.unpack x
  pp (FieldLV l x) = pp l ++ "." ++ BS.unpack x
  pp (AccessLV l exp) = pp l ++ "[" ++ exp ++ "]"

instance Printable Op where
  pp  MultOp         = "*"
  pp  DivOp          = "/"
  pp  PlusOp         = "+"
  pp  MinusOp        = "-"
  pp  EqualOp        = "="
  pp  DiffOp         = "<>"
  pp  InferiorOp     = "<"
  pp  SuperiorOp     = ">"
  pp  InferiorEqOp   = "<="
  pp  SuperiorEqOp   = ">="
  pp  AndOp          = "&"
  pp  OrOp           = "|"

instance Printable Type where
  pp (BaseT b)       = BS.unpack b
  pp (FieldsT f)     = "{" ++ pp f ++ "}"
  pp (ArrayT t)      = "array of " ++ BS.unpack t

instance Printable (DecF String) where
  pp (AliasD b t)            = "type " ++ BS.unpack b ++ "=" ++ pp t
  pp (VarD v Nothing e)      = "var " ++ BS.unpack v ++ ":=" ++ e
  pp (VarD v (Just t) e)     = "var " ++ BS.unpack v ++ ":" ++ BS.unpack t++ ":=" ++ e
  pp (FunD v f Nothing e)    = "function " ++ BS.unpack v ++ "(" ++ pp f ++ ")" ++ "=" ++ e
  pp (FunD v f (Just t) e)   = "function " ++ BS.unpack v ++ "(" ++ pp f ++ ") :" ++ BS.unpack t ++ "=" ++ e
  pp (PrimD v f Nothing)     = "primitive " ++ BS.unpack v ++ "(" ++ pp f ++ ")"
  pp (PrimD v f (Just t))    = "primitive " ++ BS.unpack v ++ "(" ++ pp f ++ ") :" ++ BS.unpack t
  pp (ImportD s)             = "import" ++ show s

instance Printable [DecF String] where
  pp x= L.intercalate ";\n" $ pp <$> x

--cata (Prelude.unwords .x) where
instance Printable (ExpF String) where
  pp (IfE c e Nothing)  = "if "++c++" then "++e
  pp (IfE c e (Just o)) = "if "++c++" then "++e++" else "++o
  pp (WhileE c e)       = "while "++c++" do "++e
  pp BreakE             = "break"
  pp (ForE v e1 e2 e3)  = "for "++BS.unpack v++":="++e1++" to "++e2++" do "++e3
  pp (LetE d e)         = "let "++pp d++" in "++L.intercalate ";\n" e++" end"
  pp (AssignE l e)      = pp l++":="++e
  pp (SeqE e)           = "("++L.intercalate ";\n" e++")"
  pp (OpE op e1 e2)     = e1++pp op++e2
  pp (MethodE l s p)    = pp l++"."++BS.unpack s++"("++L.intercalate "," p++")"
  pp (FunCallE v p)     = BS.unpack v++"("++L.intercalate "," p++")"
  pp (LValueE l)        = pp l
  pp (ArrayE b e1 e2)   = BS.unpack b++"["++e1++"]"++" of "++e2
  pp (RecordE b r)      = BS.unpack b++ "{"++L.intercalate "," ((\(w,y) -> BS.unpack w++"="++y)<$>r)++"}"
  pp NilE               = "nil"
  pp (IntegerE i)       = show i
  pp (StringE s)        = show s
