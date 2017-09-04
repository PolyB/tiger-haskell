{-# LANGUAGE OverloadedStrings #-}

module Parse.Lexer (lex) where

import Control.Monad (msum)
import Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy.Char8 as BSLC
import Parse.Tokens
import Prelude (Maybe(Nothing, Just), ($), fromIntegral, (<$>), Char, (==), return)
import Text.Parsec.Pos

type Lexer = BSL.ByteString -> Maybe (SourcePos -> (SourcePos, BSL.ByteString, Token))


lex:: BSL.ByteString -> SourcePos -> [PosToken]
lex s p = case blex s of
            Nothing -> []
            Just f -> (PosToken pos tok):(lex rest pos)
                      where (pos, rest, tok) = f p


mktok:: BSL.ByteString -> Token -> Lexer
mktok str res input = (\rest src -> (incSourceColumn src $ fromIntegral $ BSL.length str, rest, res))<$> BSL.stripPrefix str input

mktokc:: Char -> Token -> Lexer
mktokc c res input = do
                      (oc, rest) <- BSLC.uncons input
                      if oc == c then
                        return (\src -> (incSourceColumn src 1, rest, res))
                      else
                        Nothing
--mktokc c res input = (\rest src -> (incSourceColumn src 1, rest, res))<$> BSL.uncons str input

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
     ,mktokc ',' T_Comma
     ,mktokc '(' T_OParen
     ,mktokc ')' T_EParen
     ,mktok ":=" T_Assign
     ,mktokc ':' T_Colon
     ,mktokc ';' T_Semicolon
     ,mktokc '[' T_OBracket
     ,mktokc ']' T_EBracket
     ,mktokc '{' T_OBrace
     ,mktokc '}' T_EBrace
     ,mktokc '.' T_Dot
     ,mktokc '+' T_Plus
     ,mktokc '-' T_Minus
     ,mktokc '*' T_Mult
     ,mktokc '/' T_Div
     ,mktok "<>" T_Diff
     ,mktok "<=" T_InferiorEQ
     ,mktok ">=" T_SuperiorEQ
     ,mktokc '=' T_Equal
     ,mktokc '<' T_Inferior
     ,mktokc '>' T_Superior
     ,mktokc '&' T_And
     ,mktokc '|' T_Or
                ]
