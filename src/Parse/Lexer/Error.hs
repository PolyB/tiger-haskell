module Parse.Lexer.Error (err) where

import Parse.Lexer.Types
import Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy.Char8 as BSLC
import Text.Parsec.Pos
import Data.Maybe (isJust, fromJust)
import Parse.Tokens
import Data.Monoid

skipUntilOk:: ByteString -> Lexer -> (SourcePos -> SourcePos, BSL.ByteString, String)
skipUntilOk input (Lexer ref) = until 
                                (\(_,s, _)-> BSLC.null s || isJust (ref s))
                                (\(p,s, sk) -> (\(c,r) -> (srcinc 1 . p, r, c:sk))$ fromJust $ BSLC.uncons s) -- Here fromJust is ok because null is called @line 11
                                (id, input, "")

-- | call the lexer and produce an error token on error
err :: Lexer -> Lexer
err ref = ref <> (Lexer $ \s -> if BSLC.null s 
                        then Nothing -- end of input
                        else Just $ case skipUntilOk s ref of
                                      (goodPos, goodStr, skipped) -> (goodPos, goodStr, Just $ T_Err $ UnknownToken skipped))
