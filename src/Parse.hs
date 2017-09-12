{-# LANGUAGE PostfixOperators #-}

module Parse (parser) where

import Parse.TParser
import Parse.Tokens.Instance ((&))
import Parse.Tokens
import Prelude ((>>), Int, show)
import Text.Parsec.Combinator (eof)
import Text.Parsec ((<|>), token, try)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Functor ((<$))

void :: TParser a -> TParser ()
void = (<$) ()

-- TODO output the AST
parser :: TParser ()
parser = exp >> eof



exp :: TParser ()
exp = (void integer)
    <|> (try (T_Nil&))

integer :: TParser Int
integer = token (\(PosToken _ t) -> show t) pos test
            where pos   (PosToken p _)          = p
                  test  (PosToken _ (T_Int x))  = Just x
                  test _                        = Nothing
