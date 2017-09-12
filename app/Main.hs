module Main where

import Prelude (IO, (=<<), flip, (<$>), print, ($), show, putStrLn)
import Data.List as L
import Data.ByteString.Lazy as BS
import Parse
import Parse.Lexer
import Text.Parsec.Prim as P
import Text.Parsec.Pos as PS


main :: IO ()
main = parseTest parser =<< ((\x -> lex x (PS.initialPos ""))<$> BS.getContents)
