{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Parse.ParseTh (t, ti) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Text.Parsec.Pos
import Parse.Lexer as L
import Parse.Tokens as T
import Parse.Tokens.Instance as TI
import Data.Maybe
import Text.Parsec (try)


tokbind:: Token -> Q (Stmt, Maybe Name)
tokbind (T_String ('%':t)) = do
                               x <- newName "pat"
                               return $ (BindS (VarP x) (VarE $ mkName t), Just x)
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
                                  return $ LamE [VarP f] $ AppE trye $ DoE $ stmts ++ [ NoBindS $ (AppE returne) $ foldl AppE (VarE f) vars ]
                               )
}

ti :: QuasiQuoter
ti = QuasiQuoter { quoteExp = (\x -> do
                                          let QuasiQuoter{ quoteExp = n } = t
                                          aste <- n x
                                          ide <- [|id|]
                                          return $ AppE aste ide
                                  )
                                  
}
