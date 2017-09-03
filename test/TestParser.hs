{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module TestParser where

import Test.QuickCheck as Q
import Test.QuickCheck.Property as QP
import Text.Parsec.Prim as PS
import Data.ByteString.Char8
import Parse

assertParseOk :: ByteString -> QP.Property
assertParseOk s = QP.once $ QP.label ("`" ++ unpack s ++ "`") $ (case (PS.parse parser "" s ) of
                    Left err -> QP.failed { QP.reason = show err}
                    Right a -> QP.succeeded)

-- temporary tests 
-- TODO : real test suite
prop_if_1 = assertParseOk "if nil then nil else nil"
prop_if_2 = assertParseOk "if nil then nil"
prop_if_3 = assertParseOk " if nil then nil "
prop_if_4 = assertParseOk " if nil then nil else nil "
prop_if_5 = assertParseOk " if if nil then nil then nil else nil "
prop_if_6 = assertParseOk " if if nil then nil then if nil then nil else nil "
prop_par_1 = assertParseOk "( nil )"
prop_par_2 = assertParseOk "( ( ( ( nil ) ) ) )"


return []
mainParse:: IO Bool
mainParse = $(Q.quickCheckAll)
