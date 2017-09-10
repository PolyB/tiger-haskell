module Parse.Lexer.MkTok where

import Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy.Char8 as BSLC
import Parse.Lexer.Types
import Parse.Tokens

mktok:: BSL.ByteString -> Token -> Lexer
mktok str res = Lexer $ \input -> (\rest -> (srcinc $ fromIntegral $ BSL.length str, rest, Just res))<$> BSL.stripPrefix str input

mktokc:: Char -> Token -> Lexer
mktokc c res = Lexer $ \input -> do
                      (oc, rest) <- BSLC.uncons input
                      if oc == c then
                        return (srcinc 1, rest, Just res)
                      else
                        Nothing
