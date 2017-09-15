module Parse.PostFix (postfix) where

import Parse.TParser
import Text.Parsec
import Control.Monad



postfix:: [TParser (a->a)] -> TParser a -> TParser a
postfix f base = try $ do
                      x <- base
                      c <- msum $ (try . optionMaybe) <$> f
                      case c of
                        Nothing -> return x
                        Just fun  -> postfix f (return $ fun x)
