{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Parse.ParseTh (pars) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote


parsi:: String -> Q (Pat, [Exp])
parsi ['x']       = do
                  n <- newName "x"
                  return (VarP n, [VarE n])
parsi ['_']       = return (WildP, [])
parsi ('x':xs) = do
                  n <- newName "x"
                  (p,e) <- parsi xs
                  return (TupP [p, VarP n],(e ++ [VarE n]))
                  
parsi ('_':xs) = do
                  (p, e) <- parsi xs
                  return (TupP [p, WildP], e)

parsi _ = error "bad input"

                 

-- usage (\(x) -> f x) <$> [pars|x_] $ (string "A", char 'e')

bp :: String -> Name -> Q Exp
bp x n = do (p, e) <- parsi (reverse x)
            return $ LamE [p]  (foldl AppE (VarE n) e)

pars:: QuasiQuoter
pars = QuasiQuoter { quoteExp = (\x -> do
                                  n <- newName "f"
                                  r <- bp x n
                                  return $ LamE [VarP n] r),
                      quotePat = error "lol",
                     quoteType = error "lol",
                     quoteDec = error "lol"
}
