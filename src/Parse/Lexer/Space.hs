{-# LANGUAGE OverloadedStrings #-}

module Parse.Lexer.Space where

import Parse.Lexer.Types
import Data.ByteString.Lazy.Char8 as BSLC
import Control.Monad (when)

space:: Lexer
space = Lexer $ \str -> do
                          (c,r) <- BSLC.uncons str
                          when (not $ BSLC.elem c " \t") Nothing
                          return (srcinc 1, r, Nothing)
