{-# LANGUAGE PostfixOperators #-}

module Parse (parser) where

import Parse.TParser
import Parse.Tokens.Instance ((&))
import Parse.Tokens
import Prelude ((>>))
import Text.Parsec.Combinator as PS

-- TODO output the AST
parser :: TParser ()
parser = exp >> PS.eof



exp :: TParser ()
exp = (T_Nil&) 
