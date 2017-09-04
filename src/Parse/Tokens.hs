module Parse.Tokens where

import Text.Parsec.Pos
import Data.ByteString

data PosToken = PosToken SourcePos Token
           deriving (Show)

data Token = T_Int     !Int
           | T_String  String
           | T_Array
           | T_If
           | T_Then
           | T_Else
           | T_While
           | T_For
           | T_To
           | T_Do
           | T_Let
           | T_In
           | T_End
           | T_Of
           | T_Break
           | T_Nil
           | T_Function
           | T_Var
           | T_Type
           | T_Import
           | T_Primitive
           | T_Comma
           | T_Assign
           | T_Colon
           | T_Semicolon
           | T_OParen
           | T_EParen
           | T_OBracket
           | T_EBracket
           | T_OBrace
           | T_EBrace
           | T_Dot
           | T_Plus
           | T_Minus
           | T_Mult
           | T_Div
           | T_Diff
           | T_InferiorEQ
           | T_SuperiorEQ
           | T_Equal
           | T_Inferior
           | T_Superior
           | T_And
           | T_Or
           | T_Id       !ByteString
           | T_Err      ErrorTokenType
           deriving (Eq, Show)
           -- TODO : proper show instance

data ErrorTokenType = UnknownToken !String
                    | UnfinishedString
                    | NewlineInString
                    | BadEscapeCharacter
           deriving (Eq, Show)
