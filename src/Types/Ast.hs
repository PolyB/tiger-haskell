module Types.Ast where

import Data.Functor.Foldable (Fix)
import Ast (ExpF, DecF)-- base ast

data Type = TInt
         | TVoid
         | TArray Type
         | T

type TExp = Fix ExpF
type TDec = Fix DecF
