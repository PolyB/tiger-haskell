{-# LANGUAGE PostfixOperators #-}

module Parse (parser) where

import Control.Monad (msum)
import Data.Functor ((<$), ($>))
import Data.Functor.Identity
import Data.Maybe (Maybe(Just, Nothing))
import Parse.TParser
import Parse.Tokens
import Parse.Tokens.Instance ((&))
import Prelude ((>>), Int, show, String, (.), map, ($))
import Text.Parsec (token, sepBy, optionMaybe, try)
import Text.Parsec.Combinator (eof)
import Text.Parsec.Expr

-- TODO output the AST
parser :: TParser ()
parser = exp >> eof


exps :: TParser ()
exps = () <$ exp `sepBy` (T_Semicolon&)

exp :: TParser ()
exp = buildExpressionParser optable $ msum [
          () <$ ( integer )
        , () <$ ( string )
        , () <$ ( (T_Nil&) )

        , () <$ ( (T_Minus&) >> exp )
        , () <$ ( (T_OParen&) >> exp >> (T_EParen&))

        , () <$ ( (T_If&) >> exp >> (T_Then&) >> exp >> optionMaybe ( (T_Else&) >> exp) )
        , () <$ ( (T_While&) >> exp >> (T_Do&) >> exp )
        , () <$ ( (T_Break&) )
    ]

nopeop :: () -> () -> ()
nopeop _ _ = ()

optable :: OperatorTable [PosToken] () Identity ()
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
string = token (\(PosToken _ t) -> show t)  pos test
          where pos (PosToken p _)             = p
                test (PosToken _ (T_String x)) = Just x
                test _                         = Nothing

integer :: TParser Int
integer = token (\(PosToken _ t) -> show t) pos test
            where pos   (PosToken p _)          = p
                  test  (PosToken _ (T_Int x))  = Just x
                  test _                        = Nothing
