{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Parse.Lexer (lex) where

import Data.Monoid (mconcat)
import Parse.Tokens
import Data.Maybe (Maybe(Nothing, Just))
import Text.Parsec.Pos
import Data.ByteString.Lazy as BSL

import Parse.Lexer.Types
import Parse.Lexer.MkTok
import Parse.Lexer.Error
import Parse.Lexer.Eol
import Parse.Lexer.Space
import Parse.Lexer.String
import Parse.Lexer.Integer
import Parse.Lexer.Identifier
import Parse.Lexer.Comment

lex:: BSL.ByteString -> SourcePos -> [PosToken]
lex s p = case doLex (err blex) s of
            Nothing -> []
            Just (pos,rest, Nothing) -> lex rest (pos p)
            Just (pos,rest, Just tok) -> (PosToken p tok):(lex rest (pos p))

blex::  Lexer
blex = mconcat [
      eol
     ,space
     ,string
     ,comment
     ,mktok "array" T_Array
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
     ,integer
     ,identifier
                ]
