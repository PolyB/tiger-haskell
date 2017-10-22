{-# LANGUAGE OverloadedStrings #-}

module Parse.Lexer.Comment where

import Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy.Char8 as BSLC
import Parse.Lexer.Types
import Data.Maybe (fromMaybe)
import Control.Monad (msum)

comment :: Lexer
comment = Lexer $ \input -> ((\(_, rest, f, _) -> (f, rest, Nothing)) . commentParse <$>  BSL.stripPrefix "/*" input)
                where takeoneIf p str = fromMaybe str ((\(c,r) -> if p c then Just r else Nothing) =<< BSLC.uncons str)
                      commentParse str = until
                                        (\(s, rest, _, e) -> (s==0 || BSL.null rest || e))
                                        (\(s, rest, f, False) -> fromMaybe (s, rest, f, True) $ msum [
                                                                (\nr -> (s+1, nr, f . srcinc 2, False)) <$> BSL.stripPrefix "/*" rest
                                                               ,(\nr -> (s-1, nr, f . srcinc 2, False)) <$> BSL.stripPrefix "*/" rest
                                                               ,(\nr -> (s  , takeoneIf (=='\r') nr, f . srcnl, False)) <$> BSL.stripPrefix "\n" rest
                                                               ,(\nr -> (s  , takeoneIf (=='\n') nr, f . srcnl, False)) <$> BSL.stripPrefix "\r" rest
                                                               ,(\(_,r) -> (s, r, f. srcinc 1, False)) <$>BSL.uncons rest
                                                                ])
                                        (1::Integer, str, srcinc 2, False)


                  
