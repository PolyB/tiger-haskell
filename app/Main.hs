module Main where

import Data.ByteString as BS
import Parse
import Text.Parsec.Prim as P


main :: IO ()
main = P.parseTest parser =<< BS.getLine
