{-# LANGUAGE OverloadedStrings #-}

module Parse.Lexer.Eol where

import Parse.Lexer.Types
import Data.ByteString.Lazy.Char8 as BSLC
import Control.Monad (when)

eol:: Lexer
eol = Lexer $ \str -> do  (c, r) <- BSLC.uncons str
                          when (not $ BSLC.elem c "\n\r") Nothing
                          case BSLC.uncons r of
                            Just ('\n', r2) | (c == '\r') -> Just (srcnl, r2, Nothing)
                            Just ('\r', r2) | (c == '\n') -> Just (srcnl, r2, Nothing)
                            _                             -> Just (srcnl, r, Nothing)
