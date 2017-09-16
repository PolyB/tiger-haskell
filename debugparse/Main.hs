import Control.Monad (join)
import Data.ByteString.Lazy as BS (getContents)
import Parse
import Parse.Lexer
import Parse.Tokens
import Prelude (IO, print, (<$>), ($), putStrLn, Either, either, snd)
import System.Exit (exitWith, ExitCode(ExitFailure, ExitSuccess))
import Text.Parsec
import Text.Parsec.Pos as PS
import qualified Ast


perror:: ParseError -> IO ()
perror x = do
              putStrLn "ERROR : "
              print x
              exitWith $ ExitFailure 1

psuccess:: Either Ast.Exp [Ast.Dec] -> IO ()
psuccess x = do 
              putStrLn "SUCCESS : "
              print x
              exitWith ExitSuccess

main :: IO ()
main = do
        putStrLn "PARSE DEBUG : "
        tokens <- (\x -> lex x (PS.initialPos "")) <$> BS.getContents
        putStrLn " TOKENS : "
        print $ (\(PosToken _ x) -> x) <$> tokens
        either perror psuccess $ parse parser "" tokens
