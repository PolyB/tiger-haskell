module Parse.Lexer.String (string) where

import Parse.Lexer.Types
import Parse.Tokens
import Data.ByteString.Lazy.Char8 as BSLC
import Data.ByteString.Lazy as BSL
import Text.Parsec.Pos
import Data.Char as C
import Data.Maybe (fromMaybe)
import Control.Monad (unless, (>=>))

-- TODO : the string parser is ugly
--
hex:: BSL.ByteString -> Maybe (Char, BSL.ByteString, SourcePos -> SourcePos)
hex r0 = do
        (x1,r1) <- BSLC.uncons r0
        unless (C.isHexDigit x1) Nothing
        (x2,r2) <- BSLC.uncons r1
        unless (C.isHexDigit x2) Nothing
        let res = chr $ C.digitToInt x1 * 16 + C.digitToInt x2
        return (res, r2, srcinc 4)

oct:: BSL.ByteString -> Maybe (Char, BSL.ByteString, SourcePos -> SourcePos)
oct r0 = do
          (x1,r1) <- BSLC.uncons r0
          unless (C.isOctDigit x1) Nothing
          (x2,r2) <- BSLC.uncons r1
          unless (C.isOctDigit x2) Nothing
          (x3,r3) <- BSLC.uncons r2
          unless (C.isOctDigit x3) Nothing

          let res = chr $ sum [
                               C.digitToInt x1 * 64
                              ,C.digitToInt x2 * 8
                              ,C.digitToInt x3
                              ]
          return (res, r3, srcinc 4)



escapeSequence:: BSL.ByteString -> (Either ErrorTokenType (Maybe Char), BSL.ByteString, SourcePos -> SourcePos)
escapeSequence s = fromMaybe (Left UnfinishedString, s, id) $ escape <$> BSLC.uncons s
                    where escape (x,rest) = case x of
                                              '"'                 -> (Right $ Just '"', rest, srcinc 2)
                                              '\''                -> (Right $ Just '\'', rest, srcinc 2)
                                              'a'                 -> (Right $ Just '\a', rest, srcinc 2)
                                              'b'                 -> (Right $ Just '\b', rest, srcinc 2)
                                              'f'                 -> (Right $ Just '\f', rest, srcinc 2)
                                              'n'                 -> (Right $ Just '\n', rest, srcinc 2)
                                              'r'                 -> (Right $ Just '\r', rest, srcinc 2)
                                              't'                 -> (Right $ Just '\t', rest, srcinc 2)
                                              'v'                 -> (Right $ Just '\v', rest, srcinc 2)
                                              'x'                 -> case hex rest of
                                                                           Just (c, r, p) -> (Right $ Just c, r, p)
                                                                           Nothing -> (Left BadEscapeCharacter, s, srcinc 1)
                                              _ | C.isOctDigit x  -> case oct s of
                                                                           Just (c, r, p) -> (Right $ Just c, r, p)
                                                                           Nothing -> (Left BadEscapeCharacter, s, srcinc 1)

                                              _                   -> (Left BadEscapeCharacter, s, srcinc 1)

stringParser:: BSL.ByteString -> (Either ErrorTokenType (Maybe Char), BSL.ByteString, SourcePos -> SourcePos)
stringParser s = fromMaybe (Left UnfinishedString, s, id) $ (\(c, r) -> case c of 
                                                                      '\n' -> (Left NewlineInString, takeoneIf (=='\r') r, srcnl)
                                                                      '\r' -> (Left NewlineInString, takeoneIf (=='\n') r, srcnl)
                                                                      '\\' -> escapeSequence r 
                                                                      '"'  -> (Right Nothing, r,  srcinc 1)
                                                                      _    -> (Right $ Just c, r, srcinc 1)
                                                                            ) <$> BSLC.uncons s
                   where takeoneIf p str = fromMaybe str ((\(c,r) -> if p c then Just r else Nothing) =<< BSLC.uncons str)

string :: Lexer
string = Lexer (BSLC.uncons >=> (\(c,r) -> if c == '"' then Just $ stringlex r else Nothing))
                            where stringlex str = (\(f, r, t) -> (f , r, Just $ either T_Err T_String t)) $ parseiterate str
                                  parseiterate s  = case stringParser s of
                                                          (Left t, rest, f) -> (f, rest, Left t)
                                                          (Right Nothing, rest,f) -> (f, rest, Right "")
                                                          (Right (Just c), rest, f) -> case parseiterate rest of
                                                                                             (rf, rr, Right ps) -> (f . rf, rr, Right (c:ps))
                                                                                             (rf, rr, Left BadEscapeCharacter) -> (\(stringrest, r) ->(f . rf . flip (BSLC.foldl updatePosChar) stringrest, r, Left BadEscapeCharacter)) $ BSLC.break (=='"') rr
                                                                                             (rf, rr, x)  -> (f . rf, rr, x)
