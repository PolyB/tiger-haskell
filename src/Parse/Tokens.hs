{-# LANGUAGE TemplateHaskell #-}

module Parse.Tokens where

import Parse.TokenTH
import Parse.TParser
import Text.Parsec.Combinator
import Text.Parsec.Char

-- generate the definiton of tok_*name*
$(mktoks ((\a ->(a,a)) <$> [
  "array",
  "if",
  "then",
  "else",
  "while",
  "for",
  "to",
  "do",
  "let",
  "in",
  "end",
  "of",
  "break",
  "nil",
  "function",
  "var",
  "type",
  "import",
  "primitive"
          ]))

$(mktoks [
  ("comma", ","),
  ("assign", ":="),
  ("colon", ":"),
  ("semicolon", ";"),
  ("o_paren", "("),
  ("e_paren", ")"),
  ("o_bracket", "["),
  ("e_bracket", "]"),
  ("o_brace", "{"),
  ("e_brace", "}"),
  ("dot", "."),
  ("plus", "+"),
  ("minus", "-"),
  ("mult", "*"),
  ("div", "/"),
  ("diff", "<>"),
  ("inferior_eq", "<="),
  ("superior_eq", ">="),
  ("equal", "="),
  ("inferior", "<"),
  ("superior", ">"),
  ("and", "&"),
  ("or", "|")
  ])

-- TODO : add comments
tok_ignore :: TParser ()
tok_ignore = skipMany1 (oneOf " \t\n\r")

