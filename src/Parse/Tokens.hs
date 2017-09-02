{-# LANGUAGE TemplateHaskell #-}

module Parse.Tokens where

import Parse.TokenTH

-- generate the definiton of tok_*name*
$(mktoks [
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
          ])
