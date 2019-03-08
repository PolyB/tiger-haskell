import Control.Monad (join)
import Data.ByteString.Lazy as BS (getContents)
import Parse
import Parse.Lexer
import Parse.Tokens
import Prelude (IO, print, (<$>), ($), (.), putStrLn, Either, either, snd, map)
import System.Exit (exitWith, ExitCode(ExitFailure, ExitSuccess))
import Control.Monad (mapM_,fmap)
import Text.Parsec
import Text.Parsec.Pos as PS
import Ast.PrettyPrinter
import System.IO
import qualified Ast
import Utils.Annotations (ShowAll)


perror:: ParseError -> IO ()
perror x = do
              putStrLn "ERROR : "
              print x
              exitWith $ ExitFailure 1

psuccess:: ShowAll a => Either (Ast.Exp a) [Ast.Dec a] -> IO ()
psuccess x = do 
              putStrLn "SUCCESS : "
              mapM_ print x
              exitWith ExitSuccess

main :: IO ()
main = do
        hSetBuffering stdout NoBuffering
        putStrLn "PARSE DEBUG : "
        tokens <- (\x -> lex x (PS.initialPos "")) <$> BS.getContents
        putStrLn " TOKENS : "
        print $ (\(PosToken _ x) -> x) <$> tokens
        either perror (psuccess) $ parse parser "" tokens

