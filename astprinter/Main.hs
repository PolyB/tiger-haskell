import Text.Parsec.Pos as PS
import Data.ByteString.Lazy as BS (getContents)
import Parse.Lexer
import Parse
import Text.Parsec
import Ast.PrettyPrinter
import Prelude ((<$>), ($), IO, Either(Left, Right), print, putStrLn)
import Data.List (intercalate)
import Data.Functor.Foldable (unfix, refix)

main :: IO ()
main = do
        tokens <- (\x -> lex x (PS.initialPos "")) <$> BS.getContents
        case parse parser "" tokens of
          Left x -> print x
          Right (Left x) -> putStrLn $ prettyPrint $ x
          Right (Right x) -> putStrLn $ intercalate "\n" $ (prettyPrintD <$> x)
