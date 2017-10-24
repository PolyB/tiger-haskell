{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Ast where

import Data.ByteString as BS (ByteString)
import Data.Functor.Foldable
import Text.Show.Deriving

data ExpF e =  IfE       e e (Maybe e)
            |  WhileE    e e
            |  BreakE
            |  ForE      Var e e e
            |  LetE      [DecF e] [e]
            |  AssignE   (LValueF e) e
            |  SeqE      [e]
            |  OpE       Op e e
            |  MethodE   (LValueF e) BS.ByteString [e]
            |  FunCallE  Var [e]
            |  LValueE   (LValueF e)
            |  ArrayE    BaseType e e
            |  RecordE   BaseType [(BS.ByteString, e)]
            |  NilE
            |  IntegerE  Int
            |  StringE   String
              deriving (Functor, Show)

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

data LValueF e = VarLV     Var
              | FieldLV   (LValueF e) BS.ByteString
              | AccessLV  (LValueF e) e
              deriving (Functor, Show)

data Type = BaseT       BaseType
          | FieldsT     Fields
          | ArrayT      BaseType
           deriving (Eq, Show)

data DecF e = AliasD      BaseType Type
            | VarD        Var (Maybe BaseType) e
            | FunD        Var Fields (Maybe BaseType) e
            | PrimD       Var Fields (Maybe BaseType)
            | ImportD     String
            deriving (Functor, Show)


type Exp = Fix ExpF
type Dec = DecF Exp
type LValue = LValueF Exp


type Fields = [(BS.ByteString, BaseType)]
type Var = BS.ByteString
type BaseType = BS.ByteString

deriveShow1 ''DecF
deriveShow1 ''ExpF
deriveShow1 ''LValueF
