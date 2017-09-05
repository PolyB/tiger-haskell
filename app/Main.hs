module Main where

import Prelude (IO, (=<<), flip, (<$>), print, ($), show, putStrLn)
import Data.List as L
import Data.ByteString.Lazy as BS
import Parse
import Parse.Lexer
import Text.Parsec.Prim as P
import Text.Parsec.Pos as PS


main :: IO ()
main = (\t ->Prelude.putStrLn $ L.intercalate "\n" $ show <$> t)=<< ((\x -> lex x (PS.initialPos ""))<$> BS.getContents)
