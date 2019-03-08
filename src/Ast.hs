{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Ast where

import Data.Functor.Foldable
import Text.Show.Deriving
import Control.Comonad.Cofree
import Utils.Annotations

data ExpF e =  IfE       e e (Maybe e)
            |  WhileE    e e
            |  BreakE
            |  ForE      Var e e e
            |  LetE      [DecF e] [e]
            |  AssignE   (LValueF e) e
            |  SeqE      [e]
            |  OpE       Op e e
            |  MethodE   (LValueF e) String [e]
            |  FunCallE  Var [e]
            |  LValueE   (LValueF e)
            |  ArrayE    BaseType e e
            |  RecordE   BaseType [(String, e)]
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
              | FieldLV   (LValueF e) String
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


type Exp_ = Fix ExpF
type Dec_ = DecF Exp_
type LValue_ = LValueF Exp_

type Exp d = Cofree ExpF (Annotations d)
type Dec d = DecF (Exp d)
type LValue d = LValueF (Exp d)


type Fields = [(String, BaseType)]
type Var = String
type BaseType = String

deriveShow1 ''DecF
deriveShow1 ''ExpF
deriveShow1 ''LValueF
