module Parse.Tokens where

import Parse.TParser
import Text.Parsec
import Data.Functor ( ($>) )

t_array:: TParser ()
t_array = string "array" $> ()

