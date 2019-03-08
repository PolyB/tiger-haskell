{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Parse.ParseTh (t, ti, t', ti') where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Text.Parsec.Pos
import Parse.Lexer as L
import Parse.Tokens as T
import Parse.Tokens.Instance as TI
import Data.Maybe
import Text.Parsec (try)
import Control.Comonad.Cofree (Cofree(..))


tokbind:: Token -> Q (Stmt, Maybe Name)
tokbind (T_String ('%':tt)) = do
                               x <- newName "pat"
                               return $ (BindS (VarP x) (VarE $ mkName tt), Just x)
tokbind x = do
             mt <- [|TI.mktoken|]
             l <- lift x
             return (NoBindS $ AppE mt l, Nothing)

t:: QuasiQuoter
t = QuasiQuoter { quoteExp = (\x -> do
                                  (Loc fname _ _ _ _ )<- location
                                  let toks = (\(T.PosToken _ t) -> t)<$> L.lex (BSLC.pack x) (newPos fname 0 0)
                                  stmtsb <- mapM tokbind toks
                                  f <- newName "f"
                                  let stmts = fst <$> stmtsb
                                  let vars = map VarE. catMaybes $ snd <$> stmtsb
                                  trye <- [|try|]
                                  returne <- [|return|]
                                  posBeg <- newName "beg"
                                  posEnd <- newName "end"
                                  gPosE <- [|getPosition|]
                                  cobindE <- [|(:<)|]
                                  singleA <- [|singleA|]
                                  let getPosBegExp = BindS (VarP posBeg) gPosE
                                  let getPosEndExp = BindS (VarP posEnd) gPosE
                                  let annotate e = AppE (AppE cobindE $ AppE singleA $ (TupE [VarE posBeg, VarE posEnd])) e
                                  return $ LamE [VarP f] $ AppE trye $ DoE $ (getPosBegExp:stmts) ++ [ getPosEndExp , NoBindS $ (AppE returne) $ annotate $ foldl AppE (VarE f) vars ]
                               )
                  ,quotePat = fail "t must be used as an expression"
                  ,quoteType = fail "t must be used as an expression"
                  ,quoteDec = fail "t must be used as an expression"
}

t':: QuasiQuoter
t' = QuasiQuoter { quoteExp = (\x -> do
                                  (Loc fname _ _ _ _ )<- location
                                  let toks = (\(T.PosToken _ t) -> t)<$> L.lex (BSLC.pack x) (newPos fname 0 0)
                                  stmtsb <- mapM tokbind toks
                                  f <- newName "f"
                                  let stmts = fst <$> stmtsb
                                  let vars = map VarE. catMaybes $ snd <$> stmtsb
                                  trye <- [|try|]
                                  returne <- [|return|]
                                  return $ LamE [VarP f] $ AppE trye $ DoE $ stmts ++ [ NoBindS $ (AppE returne) $ foldl AppE (VarE f) vars ]
                               )
                  ,quotePat = fail "t must be used as an expression"
                  ,quoteType = fail "t must be used as an expression"
                  ,quoteDec = fail "t must be used as an expression"
}

ti :: QuasiQuoter
ti = QuasiQuoter { quoteExp = (\x -> do
                                          let QuasiQuoter{ quoteExp = n } = t
                                          aste <- n x
                                          ide <- [|id|]
                                          return $ AppE aste ide
                                  )
                  ,quotePat = fail "ti must be used as an expression"
                  ,quoteType = fail "ti must be used as an expression"
                  ,quoteDec = fail "ti must be used as an expression"
                                  
}

ti' :: QuasiQuoter
ti' = QuasiQuoter { quoteExp = (\x -> do
                                          let QuasiQuoter{ quoteExp = n } = t'
                                          aste <- n x
                                          ide <- [|id|]
                                          return $ AppE aste ide
                                  )
                  ,quotePat = fail "ti must be used as an expression"
                  ,quoteType = fail "ti must be used as an expression"
                  ,quoteDec = fail "ti must be used as an expression"
                                  
}
