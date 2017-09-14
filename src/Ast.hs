module Ast where

data Exp =  IfE       Exp Exp (Maybe Exp)
         |  WhileE    Exp Exp
         |  BreakE
         |  ForE      Var Exp Exp Exp
         |  LetE      [Dec] [Exp]
         |  AssignE   LValue Exp
         |  SeqE      [Exp]
         |  OpE       Exp Op Exp
         |  MethodE   Var [Exp]
         |  FunCallE  Var [Exp]
         |  LValueE   LValue
         |  ArrayE    BaseType Exp Exp
         |  RecordE   BaseType [(ByteString, Exp)]
         |  NilE
         |  IntegerE  Int
         |  StringE   String



data Op =  MultOp
        |  DivOp
        |  PlusOp
        |  MinusOp
        |  EqualOp
        |  DiffOp
        |  InferiorOp
        |  SuperiorOp
        |  InferiorEqOp
        |  SuperiorEqOp
        |  AndOp
        |  OrOp

data LValue = VarLV     Var
            | FieldLV   LValue ByteString
            | AccessLV  LValue Exp

data Type = BaseT       BaseType
          | FieldsT     Fields
          | ArrayT      BaseType

data Dec  = AliasD      BaseType Type
          | VarD        Var (Maybe Type) Exp
          | FunD        Var Fields (Maybe Type) Exp
          | PrimD       Var Fields (Maybe Type) Exp
          | ImportD     String

newtype Fields = [(ByteString, BaseType)]
newtype Var = !ByteString
newtype BaseType = !ByteString
