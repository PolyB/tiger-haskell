{-# LANGUAGE OverloadedStrings #-}

module Parse.Lexer.Identifier where

import Parse.Lexer.Types
import Parse.Tokens
import Data.ByteString.Lazy.Char8 as BSLC
import Data.Char as C
import Prelude ((==), (||), ($), fromIntegral)
import Data.Monoid ((<>))
import Control.Monad (unless, liftM2, return)
import Data.Maybe (Maybe(Nothing, Just))
import Parse.Lexer.MkTok

identifier:: Lexer
identifier = (mktok "_main" $ T_Id "_main" ) <> 
             (Lexer $ \str -> do 
                              (c, rs) <- BSLC.uncons str
                              unless (C.isAlpha c) Nothing
                              let (p,r) = BSLC.span (liftM2 (||) C.isAlphaNum (=='_')) rs
                              let sidentifier = BSLC.cons c p
                              return (srcinc $ fromIntegral $length sidentifier, r, Just $ T_Id $ BSLC.toStrict sidentifier))
