{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-} 

module Parse (parser) where

import Control.Monad (msum)
import Data.Functor ((<$), ($>))
import Data.Maybe (Maybe(Just, Nothing))
import Parse.TParser
import Parse.Tokens
import Parse.Tokens.Instance ((&))
import Prelude ((>>), Int, show, String, (.), map, ($))
import Text.Parsec (token, sepBy, optionMaybe, (<?>), sepBy1, many1)
import Text.Parsec.Combinator (eof)
import Text.Parsec.Expr
import Data.ByteString as BS (ByteString)

-- TODO output the AST
parser = exp >> eof


exps = () <$ exp `sepBy` (T_Semicolon&)

exp = buildExpressionParser optable $ msum [
          () <$ ( integer )
        , () <$ ( string )
        , () <$ ( (T_Nil&) )

        , () <$ ( (T_Minus&) >> exp )
        , () <$ ( (T_OParen&) >> exp >> (T_EParen&))

        , () <$ ( (T_If&) >> exp >> (T_Then&) >> exp >> optionMaybe ( (T_Else&) >> exp) )
        , () <$ ( (T_While&) >> exp >> (T_Do&) >> exp )
        , () <$ ( (T_Let&) >> decs >> (T_In&) >> exps >> (T_End&))
        , () <$ ( (T_Break&) )
    ]

decs = many1 dec

dec = msum [
          () <$ ( (T_Import&) >> string ),
          () <$ ( (T_Type&) >> identifier >> (T_Equal&) >> ty)
          ]
          

ty  = msum [
          type_id,
          ((T_Array&) >> (T_Of&) >> type_id)
        ]

tyfields = (<$) () (identifier >> (T_Colon&) >> type_id) `sepBy1` (T_Comma&)

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
identifier = token (\(PosToken _ t) -> show t) pos test
            where pos   (PosToken p _)          = p
                  test  (PosToken _ (T_Id x))   = Just x
                  test _                        = Nothing
