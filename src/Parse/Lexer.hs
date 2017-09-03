{-# LANGUAGE OverloadedStrings #-}

module Parse.Lexer (lex) where

import Data.ByteString.Lazy as BSL
import Parse.Tokens
import Prelude (Maybe(Nothing, Just), ($), fromIntegral, (<$>))
import Control.Monad (msum)
import Text.Parsec.Pos

type Lexer = BSL.ByteString -> Maybe (SourcePos -> (SourcePos, BSL.ByteString, Token))


lex:: BSL.ByteString -> SourcePos -> [PosToken]
lex s p = case blex s of
            Nothing -> []
            Just f -> (PosToken pos tok):(lex rest pos)
                      where (pos, rest, tok) = f p


mktok:: BSL.ByteString -> Token -> Lexer
mktok str res input = (\rest src -> (incSourceColumn src $ fromIntegral $ BSL.length str, rest, res))<$> BSL.stripPrefix str input

blex::  Lexer
blex str = msum $ (\x -> x str) <$> [
      mktok "array" T_Array
     ,mktok "break" T_Break
     ,mktok "do" T_Do
     ,mktok "else" T_Else
     ,mktok "end" T_End
     ,mktok "for" T_For
     ,mktok "function" T_Function
     ,mktok "if" T_If
     ,mktok "import" T_Import
     ,mktok "in" T_In
     ,mktok "let" T_Let
     ,mktok "nil" T_Nil
     ,mktok "of" T_Of
     ,mktok "primitive" T_Primitive
     ,mktok "then" T_Then
     ,mktok "to" T_To
     ,mktok "type" T_Type
     ,mktok "var" T_Var
     ,mktok "while" T_While
     ,mktok "(" T_OParen
     ,mktok ")" T_EParen
                ]
