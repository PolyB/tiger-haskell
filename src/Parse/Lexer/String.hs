module Parse.Lexer.String (string) where

import Parse.Lexer.Types
import Parse.Tokens
import Data.ByteString.Lazy.Char8 as BSLC
import Data.ByteString.Lazy as BSL
import Text.Parsec.Pos
import Data.Maybe (fromMaybe)

-- TODO : the string parser is ugly
stringParser:: BSL.ByteString -> (Either ErrorTokenType (Maybe Char), BSL.ByteString, SourcePos -> SourcePos)
stringParser s = fromMaybe (Left UnfinishedString, s, id) $ (\(c, r) -> case c of 
                                                                      '\n' -> (Left NewlineInString, takeoneIf (=='\r') r, (flip incSourceLine) 1 . (flip setSourceColumn) 0)
                                                                      '\r' -> (Left NewlineInString, takeoneIf (=='\n') r, (flip incSourceLine) 1 . (flip setSourceColumn) 0)
                                                                      '\\' -> (Left BadEscapeCharacter, r, (flip incSourceLine) 1) -- TODO
                                                                      '"'  -> (Right Nothing, r, (flip incSourceLine 1))
                                                                      _    -> (Right $ Just c, r, (flip incSourceLine) 1)
                                                                            ) <$> BSLC.uncons s
                   where takeoneIf p str = fromMaybe str $ ((\(c,r) -> if p c then Just r else Nothing) =<< BSLC.uncons str)

string :: Lexer
string = Lexer $ \input -> BSLC.uncons input >>= (\(c,r) -> if c == '"' then Just $ stringlex r else Nothing)
                            where stringlex str = (\(f, r, t) -> (f , r, Just $ either T_Err T_String t)) $ parseiterate str
                                  parseiterate s  = case stringParser s of
                                                          (Left t, rest, f) -> (f, rest, Left t)
                                                          (Right Nothing, rest,f) -> (f, rest, Right "")
                                                          (Right (Just c), rest, f) -> case parseiterate rest of
                                                                                             (rf, rr, Right ps) -> (f . rf, rr, Right (c:ps))
                                                                                             (rf, rr, Left BadEscapeCharacter) -> (\(stringrest, r) ->(f . rf . flip (BSLC.foldl updatePosChar) stringrest, r, Left BadEscapeCharacter)) $ BSLC.break (=='=') rr
                                                                                             (rf, rr, x)  -> (f . rf, rr, x)
