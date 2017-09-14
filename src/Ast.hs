module Ast where

import Data.ByteString as BS (ByteString)

data Exp =  IfE       Exp Exp (Maybe Exp)
         |  WhileE    Exp Exp
         |  BreakE
         |  ForE      Var Exp Exp Exp
         |  LetE      [Dec] [Exp]
         |  AssignE   LValue Exp
         |  SeqE      [Exp]
         |  OpE       Op Exp Exp
         |  MethodE   Var [Exp]
         |  FunCallE  Var [Exp]
         |  LValueE   LValue
         |  ArrayE    BaseType Exp Exp
         |  RecordE   BaseType [(BS.ByteString, Exp)]
         |  NilE
         |  IntegerE  Int
         |  StringE   String
           deriving (Eq, Show)



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
           deriving (Eq, Show)

data LValue = VarLV     Var
            | FieldLV   LValue BS.ByteString
            | AccessLV  LValue Exp
           deriving (Eq, Show)

data Type = BaseT       BaseType
          | FieldsT     Fields
          | ArrayT      BaseType
           deriving (Eq, Show)

data Dec  = AliasD      BaseType Type
          | VarD        Var (Maybe BaseType) Exp
          | FunD        Var Fields (Maybe BaseType) Exp
          | PrimD       Var Fields (Maybe BaseType)
          | ImportD     String
           deriving (Eq, Show)

type Fields = [(BS.ByteString, BaseType)]
type Var = BS.ByteString
type BaseType = BS.ByteString
