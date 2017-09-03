module Parse (parser) where

import Parse.TParser
import Parse.Tokens
import Text.Parsec.Prim ((<|>), try)
import Prelude (return, (>>), ($))
import Text.Parsec.Combinator as PS


(<>)::TParser a -> TParser b -> TParser ()
a <> b = do
          _ <- a
          tok_ignore
          _ <- b
          return ()

skipOptionnal:: TParser a -> TParser ()
skipOptionnal p =PS.optional $ try (tok_ignore >> p)
                  


-- TODO output the AST
parser :: TParser ()
parser = PS.optional tok_ignore >> exp >> (PS.optional tok_ignore) >> PS.eof



exp :: TParser ()
exp =     try tok_nil
      <|> (tok_minus <> exp)
      <|> (tok_o_paren <> exp <> tok_e_paren)
      <|> try (tok_if <> exp <> tok_then <> exp >> skipOptionnal (tok_else <> exp))
      <|> try (tok_while <> exp <> tok_do <> exp)
      <|> try tok_break
