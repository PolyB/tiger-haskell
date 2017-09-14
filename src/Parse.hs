{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-} 
{-# LANGUAGE QuasiQuotes #-}

module Parse (parser) where

import Control.Monad (msum)
import Data.Functor ((<$), ($>))
import Data.Maybe (Maybe(Just, Nothing))
import Parse.TParser
import Parse.Tokens
import Parse.Tokens.Instance ((&))
import Prelude ((>>), Int, show, String, (.), map, ($), const, return, (<$>))
import Text.Parsec (token, sepBy, optionMaybe, (<?>), sepBy1, many1, try)
import Text.Parsec.Combinator (eof)
import Text.Parsec.Expr
import Data.ByteString as BS (ByteString)
import qualified Ast
import Parse.ParseTh

a <+> b = do { x <- a; y <- b; return (x,y) }

-- TODO output the AST
parser = msum [
                () <$ exp
               ,() <$ decs 
               ] >> eof


exps = () <$ exp `sepBy` (T_Semicolon&)

exp = buildExpressionParser optable $ msum [
          Ast.IntegerE <$> ( integer )
        , Ast.StringE <$> ( string )
        , Ast.NilE <$ (T_Nil&)
        , (\(a,b,c)->Ast.ArrayE a b c)<$> [pars|x_x__x|] <$> try (type_id <+> (T_OBracket&) <+> exp <+> (T_EBracket&) <+> (T_Of&) <+> exp)
        , Ast.NilE <$ try (type_id >> (T_OBrace&) >> ( (identifier >> (T_Equal&) >> exp ) `sepBy` (T_Comma&) ) >> (T_EBrace&) )

        , Ast.NilE <$ try (identifier >> (T_OParen&) >> (exp `sepBy` (T_Comma&)) >> (T_EParen&))
        , Ast.NilE <$ try (lvalue >> (T_Dot&) >> identifier >> (T_OParen&) >> ( exp `sepBy` (T_Comma&)) >> (T_EParen&))

        , Ast.NilE <$ ( (T_Minus&) >> exp )
        , Ast.NilE <$ ( (T_OParen&) >> exp >> (T_EParen&))

        , Ast.NilE <$ try (lvalue >> (T_Assign&) >> exp)

        , Ast.NilE <$ ( (T_If&) >> exp >> (T_Then&) >> exp >> optionMaybe ( (T_Else&) >> exp) )
        , Ast.NilE <$ ( (T_While&) >> exp >> (T_Do&) >> exp )
        , Ast.NilE <$ ( (T_Let&) >> decs >> (T_In&) >> exps >> (T_End&))
        , Ast.NilE <$ ( (T_Break&) )
        , Ast.NilE <$ ( (T_Let&) >> decs >> (T_In&) >> exps >> (T_End&) )
        , Ast.NilE <$ lvalue
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
         ,() <$ ((T_Array&) >> (T_Of&) >> type_id)
         ,((T_OBrace&) >> tyfields >> (T_EBrace&))
        ]

tyfields = (<$) () (identifier >> (T_Colon&) >> type_id) `sepBy1` (T_Comma&)

vardec = (T_Var&) >> identifier >> optionMaybe ((T_Colon&) >> type_id) >> (T_Assign&) >> exp

type_id:: TParser Ast.BaseType
type_id = identifier

nopeop _ _ = ()

optable = (map . map) (\(x,y,z)-> Infix (y $> Ast.OpE z) x) [
            [
              (AssocLeft, (T_Mult&), Ast.MultOp),
              (AssocLeft, (T_Div&), Ast.DivOp)
            ],
            [
              (AssocLeft, (T_Plus&), Ast.PlusOp),
              (AssocLeft, (T_Minus&), Ast.MinusOp)
            ],
            [
              (AssocNone, (T_Equal&), Ast.EqualOp),
              (AssocNone, (T_Diff&), Ast.DiffOp),
              (AssocNone, (T_Inferior&), Ast.InferiorOp),
              (AssocNone, (T_Superior&), Ast.SuperiorOp),
              (AssocNone, (T_InferiorEQ&), Ast.InferiorEqOp),
              (AssocNone, (T_SuperiorEQ&), Ast.SuperiorEqOp)
            ],
            [(AssocLeft, (T_And&), Ast.AndOp)],
            [(AssocLeft, (T_Or&), Ast.OrOp)]
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
