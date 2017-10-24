{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-} 
{-# LANGUAGE QuasiQuotes #-}

module Parse (parser) where

import Control.Monad (msum)
import Data.Functor ((<$), ($>))
import Data.Maybe (Maybe(Just, Nothing))
import Parse.TParser
import Parse.Tokens
import Data.Functor.Foldable (Fix(Fix))
import Parse.Tokens.Instance ((&))
import Prelude ((>>), Int, show, String, (.), map, ($), return, (<$>))
import Text.Parsec (token, sepBy, optionMaybe, (<?>), many, try)
import Text.Parsec.Combinator (eof)
import Text.Parsec.Expr
import Data.ByteString as BS (ByteString)
import qualified Ast
import Parse.ParseTh
import Data.Either
import Parse.PostFix
import Data.Functor.Identity

a <+> b = do { x <- a; y <- b; return (x,y) }
a << b = do { x <- a ; _ <- b; return x }

parser :: TParser (Either Ast.Exp [Ast.Dec])
parser = msum [
                Left  <$> exp
               ,Right <$> decs 
               ] << eof


exps = exp `sepBy` (T_Semicolon&)

exp:: TParser Ast.Exp
exp = buildExpressionParser optable $ Fix <$> msum [
          Ast.IntegerE                                <$> integer
        , Ast.StringE                                 <$> string
        , Ast.NilE                                    <$  (T_Nil&)
        , [pars|x_x__x|] Ast.ArrayE                   <$> try (type_id <+> (T_OBracket&) <+> exp <+> (T_EBracket&) <+> (T_Of&) <+> exp)
        , [pars|x_x_|] Ast.RecordE                    <$> try (type_id <+> (T_OBrace&) <+> ( ([pars|x_x|] (,) <$>(identifier <+> (T_Equal&) <+> exp )) `sepBy` (T_Comma&) ) <+> (T_EBrace&) )

        , [pars|x_x_|] Ast.FunCallE                   <$> try (identifier <+> (T_OParen&) <+> (exp `sepBy` (T_Comma&)) <+> (T_EParen&))
        , [pars|x_x_x_|] Ast.MethodE                  <$> try (lvalue <+> (T_Dot&) <+> identifier <+> (T_OParen&) <+> ( exp `sepBy` (T_Comma&)) <+> (T_EParen&))

        , Ast.OpE Ast.MinusOp (Fix $ Ast.IntegerE 0)  <$> ((T_Minus&) >> exp)
        , [pars|_x_|] Ast.SeqE                        <$> ((T_OParen&) <+> exps <+> (T_EParen&))

        , [pars|x_x|] Ast.AssignE                     <$> try (lvalue <+> (T_Assign&) <+> exp)

        , [pars|_x_xx|] Ast.IfE                       <$> ((T_If&) <+> exp <+> (T_Then&) <+> exp <+> optionMaybe ( (T_Else&) >> exp) )
        , [pars|_x_x|] Ast.WhileE                     <$> ((T_While&) <+> exp <+> (T_Do&) <+> exp )
        , [pars|_x_x_|] Ast.LetE                      <$> ((T_Let&) <+> decs <+> (T_In&) <+> exps <+> (T_End&))
        , [pars|_x_x_x_x|]  Ast.ForE                  <$> ((T_For&) <+> identifier <+> (T_Assign&) <+> exp <+> (T_To&) <+> exp <+> (T_Do&) <+> exp)
        , Ast.BreakE                                  <$  (T_Break&)
        , Ast.LValueE                                 <$> lvalue
    ]

lvalue = postfix [
                    (do 
                          (T_Dot&)
                          x <- identifier
                          return $ \v -> Ast.FieldLV v x),
                    (do
                         (T_OBracket&)
                         x <- exp
                         (T_EBracket&)
                         return $ \v -> Ast.AccessLV v x)

                  ] (( Ast.VarLV <$> identifier) <?> "lvalue")

decs = many dec
dec = msum [
           [pars|_x_x|] Ast.AliasD    <$> ( (T_Type&) <+> identifier <+> (T_Equal&) <+> ty)
          ,vardec
          ,[pars|_x_x_x_x|] Ast.FunD  <$> ( (T_Function&) <+> identifier <+> (T_OParen&) <+> tyfields <+> (T_EParen&) <+> optionMaybe ( (T_Colon&) >> type_id ) <+> (T_Equal&) <+> exp )
          ,[pars|_x_x_x|] Ast.PrimD   <$> ( (T_Primitive&) <+> identifier <+> (T_OParen&) <+> tyfields <+> (T_EParen&) <+> optionMaybe ( (T_Colon&) >> type_id ))
          , Ast.ImportD <$> ( (T_Import&) >> string )
          ]
          

ty  = msum [
          Ast.BaseT   <$> type_id
         ,Ast.ArrayT  <$> ((T_Array&) >> (T_Of&) >> type_id)
         ,[pars|_x_|] Ast.FieldsT <$> ((T_OBrace&) <+> tyfields <+> (T_EBrace&))
        ]

tyfields = ([pars|x_x|] (,) <$> (identifier <+> (T_Colon&) <+> type_id)) `sepBy` (T_Comma&)

vardec = [pars|_xx_x|] Ast.VarD <$> ((T_Var&) <+> identifier <+> optionMaybe ((T_Colon&) >> type_id) <+> (T_Assign&) <+> exp)

type_id:: TParser Ast.BaseType
type_id = identifier <?> "type_id"

optable:: OperatorTable [PosToken] () Identity Ast.Exp
optable = (map . map) (\(x,y,z)-> Infix (y $> (\l r ->Fix$Ast.OpE z l r)) x) [
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
