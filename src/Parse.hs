{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-} 
{-# LANGUAGE QuasiQuotes #-}

module Parse (parser) where

import Control.Monad (msum)
import Data.Functor (($>))
import Data.Maybe (Maybe(Just, Nothing))
import Parse.TParser
import Parse.Tokens
import Data.Functor.Foldable (Fix(Fix))
import Parse.Tokens.Instance ((&))
import Prelude ((>>), Int, show, String, (.), map, ($), return, (<$>))
import Text.Parsec (token, sepBy, optionMaybe, (<?>), many)
import Text.Parsec.Combinator (eof)
import Text.Parsec.Expr
import qualified Ast
import Parse.ParseTh
import Data.Either
import Parse.PostFix
import Data.Functor.Identity

a << b = do { x <- a ; _ <- b; return x }

parser :: TParser (Either Ast.Exp [Ast.Dec])
parser = msum [
                Left  <$> exp
               ,Right <$> decs 
               ] << eof


exps = exp `sepBy` (T_Semicolon&)

exp:: TParser Ast.Exp
exp = buildExpressionParser optable $ Fix <$> msum [
          [t|"%integer"|] Ast.IntegerE
        , [t|"%string"|] Ast.StringE
        , [t|nil|] Ast.NilE
        , [t|"%type_id" [ "%exp" ] of "%exp" |] Ast.ArrayE
        , [t|"%type_id" { "%recs" }|] Ast.RecordE
        , [t| "%identifier" ( "%args" )|] Ast.FunCallE
        , [t|"%lvalue"."%identifier"("%args")|] Ast.MethodE
        , [t| - "%exp" |] (Ast.OpE Ast.MinusOp (Fix $ Ast.IntegerE 0))
        , [t|("%exps")|] Ast.SeqE
        , [t|"%lvalue" := "%exp" |] Ast.AssignE
        , [t|if "%exp" then "%exp" "%elsee"|] Ast.IfE
        , [t|while "%exp" do "%exp"|] Ast.WhileE
        , [t|let "%decs" in "%exps" end|] Ast.LetE
        , [t|for "%identifier" := "%exp" to "%exp" do "%exp" |]  Ast.ForE
        , [t|break|] Ast.BreakE
        , [t|"%lvalue"|] Ast.LValueE
    ]
    where args = exp `sepBy` [ti|,|]
          rec_entry = [t|"%identifier" = "%exp"|] (,)
          recs = rec_entry `sepBy` [ti|,|]
          elsee = optionMaybe $ [ti|else "%exp"|]

lvalue = postfix [
                    do 
                          (T_Dot&)
                          x <- identifier
                          return $ \v -> Ast.FieldLV v x,
                    do
                         (T_OBracket&)
                         x <- exp
                         (T_EBracket&)
                         return $ \v -> Ast.AccessLV v x

                  ] (( Ast.VarLV <$> identifier) <?> "lvalue")

decs = many dec
dec = msum [
            [t|type "%identifier" = "%ty"|] Ast.AliasD
          , vardec
          , [t|function "%identifier" ( "%tyfields" ) "%opt_type" = "%exp"|] Ast.FunD
          , [t|primitive "%identifier" ( "%tyfields" ) "%opt_type" |] Ast.PrimD
          , [t|import "%string"|] Ast.ImportD
          ]
          where opt_type = optionMaybe $ [ti|: "%type_id"|]
          

ty  = msum [
          Ast.BaseT   <$> type_id
         ,Ast.ArrayT  <$> ((T_Array&) >> (T_Of&) >> type_id)
         ,[t|{ "%tyfields" }|]  Ast.FieldsT
        ]

tyfields = [t|"%identifier" : "%type_id"|] (,) `sepBy` [ti|,|]

vardec = [t|var "%identifier" "%opt_type" = "%exp" |] Ast.VarD
  where opt_type = optionMaybe [ti| : "%type_id"|]



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

identifier :: TParser String
identifier = token (\(PosToken _ t) -> show t) pos test <?> "identifier"
            where pos   (PosToken p _)          = p
                  test  (PosToken _ (T_Id x))   = Just x
                  test _                        = Nothing
