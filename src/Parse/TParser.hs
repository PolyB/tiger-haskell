module Parse.TParser where

import Text.Parsec.Prim
import Data.Functor.Identity
import Parse.Tokens

type TParser = ParsecT [PosToken] () Identity
