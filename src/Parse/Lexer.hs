{-# LANGUAGE OverloadedStrings #-}

module Parse.Lexer (lex) where

import Control.Monad (msum, when)
import Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy.Char8 as BSLC
import Parse.Tokens
import Prelude (Maybe(Nothing, Just), ($), fromIntegral, (<$>), Char, (==), return, not, flip, until, String, (||), )
import Data.Maybe (isJust, fromJust)
import Text.Parsec.Pos

type Lexer = BSL.ByteString -> Maybe (SourcePos -> (SourcePos, BSL.ByteString, Maybe Token))


lex:: BSL.ByteString -> SourcePos -> [PosToken]
lex s p = case (err blex) s of
            Nothing -> []
            Just f -> case f p of 
                      (pos, rest, Nothing) -> (lex rest pos)
                      (pos, rest, Just tok) -> (PosToken p tok):(lex rest pos)


mktok:: BSL.ByteString -> Token -> Lexer
mktok str res input = (\rest src -> (incSourceColumn src $ fromIntegral $ BSL.length str, rest, Just res))<$> BSL.stripPrefix str input

mktokc:: Char -> Token -> Lexer
mktokc c res input = do
                      (oc, rest) <- BSLC.uncons input
                      if oc == c then
                        return (\src -> (incSourceColumn src 1, rest, Just res))
                      else
                        Nothing
--mktokc c res input = (\rest src -> (incSourceColumn src 1, rest, res))<$> BSL.uncons str input

eol:: Lexer
eol str = do  (c, r) <- BSLC.uncons str
              when (not $ BSLC.elem c "\n\r") Nothing
              case BSLC.uncons r of
                Just ('\n', r2) | (c == '\r') -> Just (\s -> (flip setSourceColumn 0 $ incSourceLine s 1, r2, Nothing))
                Just ('\r', r2) | (c == '\n') -> Just (\s -> (flip setSourceColumn 0 $ incSourceLine s 1, r2, Nothing))
                _                             -> Just (\s -> (flip setSourceColumn 0 $ incSourceLine s 1, r, Nothing))
space:: Lexer
space str = do
              (c,r) <- BSLC.uncons str
              when (not $ BSLC.elem c " \t") Nothing
              return (\s -> (incSourceColumn s 1, r, Nothing))
                            
skipUntilOk:: ByteString -> Lexer -> SourcePos -> (SourcePos, BSL.ByteString, String)
skipUntilOk input ref pos = until 
                              (\(_,s, _)-> BSLC.null s || (isJust $ ref s)) 
                              (\(p,s, sk) -> (\(c,r) -> (incSourceColumn p 1, r, c:sk))$ fromJust $ BSLC.uncons s)
                              (pos, input, "")

-- | call the lexer and produce an error token on error
err :: Lexer -> Lexer
err f s = if BSLC.null s then Nothing else 
                  Just $ case blex s of
                    (Just v) -> v
                    Nothing -> (\startpos -> (\(goodPos, goodStr, skipped) -> (goodPos, goodStr, Just $ T_Err $ skipped)) $ skipUntilOk s f startpos)
blex::  Lexer
blex str = msum $ (\x -> x str) <$> [
      eol
     ,space
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
                ]
