module Parse.Lexer.Types where

import Control.Monad (mplus)
import Data.ByteString.Lazy as BSL
import Parse.Tokens
import Text.Parsec.Pos

newtype Lexer = Lexer (BSL.ByteString -> Maybe (SourcePos -> SourcePos, BSL.ByteString, Maybe Token))

srcinc:: Int -> SourcePos -> SourcePos
srcinc = flip incSourceColumn

srcnl:: SourcePos -> SourcePos
srcnl s = setSourceColumn (incSourceLine s 1) 0

instance Monoid Lexer where
 mempty = Lexer $ const Nothing
 mappend (Lexer f) (Lexer f2) = Lexer $ \s -> mplus (f s) (f2 s)

doLex:: Lexer -> BSL.ByteString -> Maybe (SourcePos->SourcePos, BSL.ByteString, Maybe Token)
doLex (Lexer l) = l

tmap :: (Token-> Token) -> Lexer -> Lexer
tmap f (Lexer l) = Lexer $ \s -> (\(p, i, t) -> (p,i , f<$> t)) <$> l s

