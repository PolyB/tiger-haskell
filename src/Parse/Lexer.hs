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
     ,keywords
                ]

keywords :: Lexer
keywords = tmap keywords_match identifier

keywords_match:: Token -> Token
keywords_match (T_Id "array"    ) = T_Array
keywords_match (T_Id "break"    ) = T_Break
keywords_match (T_Id "do"       ) = T_Do
keywords_match (T_Id "else"     ) = T_Else
keywords_match (T_Id "end"      ) = T_End
keywords_match (T_Id "for"      ) = T_For
keywords_match (T_Id "function" ) = T_Function
keywords_match (T_Id "if"       ) = T_If
keywords_match (T_Id "import"   ) = T_Import
keywords_match (T_Id "in"       ) = T_In
keywords_match (T_Id "let"      ) = T_Let
keywords_match (T_Id "nil"      ) = T_Nil
keywords_match (T_Id "of"       ) = T_Of
keywords_match (T_Id "primitive") = T_Primitive
keywords_match (T_Id "then"     ) = T_Then
keywords_match (T_Id "to"       ) = T_To
keywords_match (T_Id "type"     ) = T_Type
keywords_match (T_Id "var"      ) = T_Var
keywords_match (T_Id "while"    ) = T_While
keywords_match a = a
