{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Parse.Tokens.Instance ((&)) where

import Parse.TParser
import Parse.Tokens
import Text.Parsec (token)
import Data.ByteString.Char8 as B

class Tokenable t s | t -> s where
  mktoken :: t -> TParser s

instance Tokenable Token () where
  mktoken T_Nil         = mkmktoken "nil" T_Nil
  mktoken T_If          = mkmktoken "if" T_If
  mktoken T_OParen      = mkmktoken "`(`" T_OParen
  mktoken T_EParen      = mkmktoken "`)`" T_EParen
  mktoken T_Then        = mkmktoken "then" T_Then
  mktoken T_Array       = mkmktoken "array" T_Array
  mktoken T_Else        = mkmktoken "else" T_Else
  mktoken T_While       = mkmktoken "while" T_While
  mktoken T_For         = mkmktoken "for" T_For
  mktoken T_To          = mkmktoken "to" T_To
  mktoken T_Do          = mkmktoken "do" T_Do
  mktoken T_Let         = mkmktoken "let" T_Let
  mktoken T_In          = mkmktoken "in" T_In
  mktoken T_End         = mkmktoken "end" T_End
  mktoken T_Of          = mkmktoken "of" T_Of
  mktoken T_Break       = mkmktoken "break" T_Break
  mktoken T_Function    = mkmktoken "function" T_Function
  mktoken T_Var         = mkmktoken "var" T_Var
  mktoken T_Type        = mkmktoken "type" T_Type
  mktoken T_Import      = mkmktoken "import" T_Import
  mktoken T_Primitive   = mkmktoken "primitive" T_Primitive
  mktoken T_Comma       = mkmktoken "`,`" T_Comma
  mktoken T_Assign      = mkmktoken "`:=`" T_Assign
  mktoken T_Colon       = mkmktoken "`:`" T_Colon
  mktoken T_Semicolon   = mkmktoken "`;`" T_Semicolon
  mktoken T_OBracket    = mkmktoken "`[`" T_OBracket
  mktoken T_EBracket    = mkmktoken "`]`" T_EBracket
  mktoken T_OBrace      = mkmktoken "`{`" T_OBrace
  mktoken T_EBrace      = mkmktoken "`}`" T_EBrace
  mktoken T_Dot         = mkmktoken "`.`" T_Dot
  mktoken T_Plus        = mkmktoken "`+`" T_Plus
  mktoken T_Minus       = mkmktoken "`-`" T_Minus
  mktoken T_Mult        = mkmktoken "`*`" T_Mult
  mktoken T_Div         = mkmktoken "`/`" T_Div
  mktoken T_Diff        = mkmktoken "`<>`" T_Diff
  mktoken T_InferiorEQ  = mkmktoken "`<=`" T_InferiorEQ
  mktoken T_SuperiorEQ  = mkmktoken "`>=" T_SuperiorEQ
  mktoken T_Equal       = mkmktoken "`=`" T_Equal
  mktoken T_Inferior    = mkmktoken "`<`" T_Inferior
  mktoken T_Superior    = mkmktoken "`>`" T_Superior
  mktoken T_And         = mkmktoken "`&`" T_And
  mktoken T_Or          = mkmktoken "`|`" T_Or
  mktoken v@(T_Id i)    = mkmktoken (B.unpack i) v
  mktoken v@(T_String s)= mkmktoken ("\"" ++ B.unpack s ++ "\"") v
  mktoken v@(T_Int i)   = mkmktoken (show i) v

(&) ::Tokenable t a => t -> TParser a
(&) = mktoken

mkmktoken:: String -> Token -> TParser ()
mkmktoken s t = token (const s) (\(PosToken p _) -> p) (\(PosToken _ v) -> if v == t then Just () else Nothing)
