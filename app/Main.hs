module Main where

import Prelude (IO, (=<<), flip, (<$>), print)
import Data.ByteString.Lazy as BS
import Parse
import Parse.Lexer
import Text.Parsec.Prim as P
import Text.Parsec.Pos as PS


main :: IO ()
main = print =<< ((\x -> lex x (PS.initialPos ""))<$> BS.getContents)
