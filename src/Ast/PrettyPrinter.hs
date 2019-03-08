{-# LANGUAGE FlexibleInstances #-}
module Ast.PrettyPrinter (prettyPrint, prettyPrintD) where
--import Data.ByteString as BS
import Ast
import Data.ByteString.Char8 as BS
import Data.Functor.Foldable
import Data.List as L
import Prelude ((++), (<$>), String, Maybe(Just, Nothing), show, ($))
import Data.Functor (Functor, fmap)
import Control.Comonad.Trans.Cofree
import Data.Functor.Identity

prettyPrint::Exp a -> String
prettyPrint f = cata pp f

prettyPrintD::Dec a -> String
prettyPrintD f = pp (fmap prettyPrint f)

class Printable r where
  pp :: r -> String

instance Printable Fields where
  pp f = L.intercalate "," ((\(x,y) -> x ++ ":" ++ y) <$> f)


instance Printable (e String) => Printable (CofreeF e ann String) where
  pp (_ :< d) = pp d
  

instance Printable (LValueF String) where
  pp (VarLV x) = x
  pp (FieldLV l x) = pp l ++ "." ++ x
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
  pp (BaseT b)       = b
  pp (FieldsT f)     = "{" ++ pp f ++ "}"
  pp (ArrayT t)      = "array of " ++ t

instance Printable (DecF String) where
  pp (AliasD b t)            = "type " ++  b ++ "=" ++ pp t
  pp (VarD v Nothing e)      = "var " ++  v ++ ":=" ++ e
  pp (VarD v (Just t) e)     = "var " ++  v ++ ":" ++  t++ ":=" ++ e
  pp (FunD v f Nothing e)    = "function " ++  v ++ "(" ++ pp f ++ ")" ++ "=" ++ e
  pp (FunD v f (Just t) e)   = "function " ++  v ++ "(" ++ pp f ++ ") :" ++  t ++ "=" ++ e
  pp (PrimD v f Nothing)     = "primitive " ++  v ++ "(" ++ pp f ++ ")"
  pp (PrimD v f (Just t))    = "primitive " ++  v ++ "(" ++ pp f ++ ") :" ++  t
  pp (ImportD s)             = "import" ++ show s

instance Printable [DecF String] where
  pp x= L.intercalate ";\n" $ pp <$> x

--cata (Prelude.unwords .x) where
instance Printable (ExpF String) where
  pp (IfE c e Nothing)  = "if "++c++" then "++e
  pp (IfE c e (Just o)) = "if "++c++" then "++e++" else "++o
  pp (WhileE c e)       = "while "++c++" do "++e
  pp BreakE             = "break"
  pp (ForE v e1 e2 e3)  = "for "++ v++":="++e1++" to "++e2++" do "++e3
  pp (LetE d e)         = "let "++pp d++" in "++L.intercalate ";\n" e++" end"
  pp (AssignE l e)      = pp l++":="++e
  pp (SeqE e)           = "("++L.intercalate ";\n" e++")"
  pp (OpE op e1 e2)     = e1++pp op++e2
  pp (MethodE l s p)    = pp l++"."++ s++"("++L.intercalate "," p++")"
  pp (FunCallE v p)     =  v++"("++L.intercalate "," p++")"
  pp (LValueE l)        = pp l
  pp (ArrayE b e1 e2)   =  b++"["++e1++"]"++" of "++e2
  pp (RecordE b r)      =  b++ "{"++L.intercalate "," ((\(w,y) ->  w++"="++y)<$>r)++"}"
  pp NilE               = "nil"
  pp (IntegerE i)       = show i
  pp (StringE s)        = show s
