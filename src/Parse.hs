module Parse (parser) where

import Parse.TParser
import Parse.Tokens
import Text.Parsec.Prim ((<|>), try)
import qualified Prelude
import Text.Parsec.Combinator as PS


(<>)::TParser a -> TParser b -> TParser ()
a <> b = do
          _ <- a
          tok_ignore
          _ <- b
          Prelude.return ()


-- TODO output the AST
parser :: TParser ()
parser = exp



exp :: TParser ()
exp =     try tok_nil
      <|> (tok_minus <> exp)
      <|> (tok_o_paren <> exp <> tok_e_paren)
      <|> try (tok_if <> exp <> tok_then <> exp <> PS.optional (tok_else <> exp))
      <|> try (tok_while <> exp <> tok_do <> exp)
      <|> try tok_break
