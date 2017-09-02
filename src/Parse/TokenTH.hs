{-# LANGUAGE TemplateHaskell #-}

module Parse.TokenTH where

import Parse.TParser
import qualified Text.Parsec
import Language.Haskell.TH
import Data.Functor ( ($>) )

mktoks:: [String] -> Q [Dec]
mktoks tokens = do
  let vname t = mkName ("tok_" ++ t)
  expType <- [t| TParser ()|]
  pattern <- [| \x -> Text.Parsec.string x $> ()|]
  return $ [
      \t -> SigD (vname t) expType,
      \t -> ValD (VarP (vname t)) (NormalB (AppE pattern (LitE (StringL t)))) [] 
      ] <*>  tokens

