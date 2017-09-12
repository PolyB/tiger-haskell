{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Parse.Tokens.Instance ((&)) where

import Parse.TParser
import Parse.Tokens
import Text.Parsec (token)

class Tokenable t s | t -> s where
  mktoken :: t -> TParser s

instance Tokenable Token () where
  mktoken t = token (\(PosToken _ v) -> show v) (\(PosToken p _) -> p) (\(PosToken _ v) -> if v == t then Just () else Nothing)

(&) ::Tokenable t a => t -> TParser a
(&) = mktoken
