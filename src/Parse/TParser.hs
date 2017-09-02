module Parse.TParser where

import Data.ByteString
import Text.Parsec.Prim
import Data.Functor.Identity

type TParser = ParsecT ByteString () Identity
