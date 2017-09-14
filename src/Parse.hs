{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-} 

module Parse (parser) where

import Control.Monad (msum)
import Data.Functor ((<$), ($>))
import Data.Maybe (Maybe(Just, Nothing))
import Parse.TParser
import Parse.Tokens
import Parse.Tokens.Instance ((&))
import Prelude ((>>), Int, show, String, (.), map, ($), const)
import Text.Parsec (token, sepBy, optionMaybe, (<?>), sepBy1, many1, try)
import Text.Parsec.Combinator (eof)
import Text.Parsec.Expr
import Data.ByteString as BS (ByteString)

-- TODO output the AST
parser = msum [
                exp
               ,() <$ decs 
               ] >> eof


exps = () <$ exp `sepBy` (T_Semicolon&)

exp = buildExpressionParser optable $ msum [
          () <$ ( integer )
        , () <$ ( string )
        , () <$ ( (T_Nil&) )
        , () <$ try (type_id >> (T_OBracket&) >> exp >> (T_EBracket&) >> (T_Of&) >> exp)
        , () <$ try (type_id >> (T_OBrace&) >> ( (identifier >> (T_Equal&) >> exp ) `sepBy` (T_Comma&) ) >> (T_EBrace&) )

        , () <$ try (identifier >> (T_OParen&) >> (exp `sepBy` (T_Comma&)) >> (T_EParen&))
        , () <$ try (lvalue >> (T_Dot&) >> identifier >> (T_OParen&) >> ( exp `sepBy` (T_Comma&)) >> (T_EParen&))

        , () <$ ( (T_Minus&) >> exp )
        , () <$ ( (T_OParen&) >> exp >> (T_EParen&))

        , () <$ try (lvalue >> (T_Assign&) >> exp)

        , () <$ ( (T_If&) >> exp >> (T_Then&) >> exp >> optionMaybe ( (T_Else&) >> exp) )
        , () <$ ( (T_While&) >> exp >> (T_Do&) >> exp )
        , () <$ ( (T_Let&) >> decs >> (T_In&) >> exps >> (T_End&))
        , () <$ ( (T_Break&) )
        , () <$ ( (T_Let&) >> decs >> (T_In&) >> exps >> (T_End&) )
        , () <$ lvalue
    ]

lvalue = buildExpressionParser [[
                                   Infix ((T_Dot&) $> nopeop) AssocLeft
                                  ,Postfix (((T_OBracket&) >> exp >> (T_EBracket&)) $> const ())
                                ]] (() <$ identifier)

decs = many1 dec
dec = msum [
           () <$ ( (T_Type&) >> identifier >> (T_Equal&) >> ty)
          ,() <$ vardec
          ,() <$ ( (T_Function&) >> identifier >> (T_OParen&) >> tyfields >> (T_EParen&) >> optionMaybe ( (T_Colon&) >> type_id ) >> (T_Equal&) >> exp )
          ,() <$ ( (T_Primitive&) >> identifier >> (T_OParen&) >> tyfields >> (T_EParen&) >> optionMaybe ( (T_Colon&) >> type_id ))
          ,() <$ ( (T_Import&) >> string )
          ]
          

ty  = msum [
          () <$ type_id
         ,((T_Array&) >> (T_Of&) >> type_id)
         ,((T_OBrace&) >> tyfields >> (T_EBrace&))
        ]

tyfields = (<$) () (identifier >> (T_Colon&) >> type_id) `sepBy1` (T_Comma&)

vardec = (T_Var&) >> identifier >> optionMaybe ((T_Colon&) >> type_id) >> (T_Assign&) >> exp

type_id = () <$ identifier

nopeop _ _ = ()

optable = (map . map) (\(x,y)-> Infix y x) [
            [
              (AssocLeft, (T_Mult&) $> nopeop ),
              (AssocLeft, (T_Div&) $> nopeop)
            ],
            [
              (AssocLeft, (T_Plus&) $> nopeop),
              (AssocLeft, (T_Minus&) $> nopeop)
            ],
            [
              (AssocNone, (T_Equal&) $> nopeop),
              (AssocNone, (T_Diff&) $> nopeop),
              (AssocNone, (T_Inferior&) $> nopeop),
              (AssocNone, (T_Superior&) $> nopeop),
              (AssocNone, (T_InferiorEQ&) $> nopeop),
              (AssocNone, (T_SuperiorEQ&) $> nopeop)
            ],
            [(AssocLeft, (T_And&) $> nopeop)],
            [(AssocLeft, (T_Or&) $> nopeop)]
          ]


string :: TParser String
string = token (\(PosToken _ t) -> show t)  pos test <?> "string"
          where pos (PosToken p _)             = p
                test (PosToken _ (T_String x)) = Just x
                test _                         = Nothing

integer :: TParser Int
integer = token (\(PosToken _ t) -> show t) pos test <?> "integer"
            where pos   (PosToken p _)          = p
                  test  (PosToken _ (T_Int x))  = Just x
                  test _                        = Nothing

identifier :: TParser BS.ByteString
identifier = token (\(PosToken _ t) -> show t) pos test <?> "identifier"
            where pos   (PosToken p _)          = p
                  test  (PosToken _ (T_Id x))   = Just x
                  test _                        = Nothing
