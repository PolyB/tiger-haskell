module Parse.Lexer.Integer where

import Parse.Lexer.Types
import Parse.Tokens
import Data.ByteString.Lazy.Char8 as BSLC
import Data.Char as C

integer:: Lexer
integer = Lexer $ \s -> (\(f,rest) -> if BSLC.null f 
                                      then Nothing 
                                      else Just (srcinc (fromIntegral$BSLC.length f), rest, Just $ T_Int $ read $ BSLC.unpack f))$ BSLC.span C.isDigit s
