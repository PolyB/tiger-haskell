{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Parse.Tokens.Instance where

import Parse.TParser
import Parse.Tokens
import Text.Parsec (token)

class Tokenable t s | t -> s where
  mktoken :: t -> TParser s

instance Tokenable Token () where
  mktoken T_Nil = token (const "nil") (\(PosToken p _) -> p) (\(PosToken _ v) -> if v == T_Nil then Just () else Nothing)
  mktoken T_If = token (const "if") (\(PosToken p _) -> p) (\(PosToken _ v) -> if v == T_If then Just () else Nothing)
  mktoken T_OParen = token (const "`(`") (\(PosToken p _) -> p) (\(PosToken _ v) -> if v == T_OParen then Just () else Nothing)
  mktoken T_EParen = token (const "`)`") (\(PosToken p _) -> p) (\(PosToken _ v) -> if v == T_EParen then Just () else Nothing)
                      

(&) ::Tokenable t a => t -> TParser a
(&) = mktoken
