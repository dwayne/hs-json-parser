{-# LANGUAGE OverloadedStrings #-}

module Test.Json.Parser2Spec (spec) where

import qualified Json.Parser2 as P

import Data.Text (Text)
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec (parse, eof)


spec :: Spec
spec =
  describe "Parser" $ do
    wsSpec


wsSpec :: Spec
wsSpec =
  describe "ws1" $ do
    it "zero" $ do
      parseTillEnd P.ws1 `shouldFailOn` ""

    it "one" $ do
      parseTillEnd P.ws1 `shouldSucceedOn` " "
      parseTillEnd P.ws1 `shouldSucceedOn` "\t"
      parseTillEnd P.ws1 `shouldSucceedOn` "\n"
      parseTillEnd P.ws1 `shouldSucceedOn` "\r"

    it "more than one" $ do
      parseTillEnd P.ws1 `shouldSucceedOn` " \n\r\t\t\r\n "


-- Helpers


parseTillEnd :: P.Parser a -> Text -> Either P.Error a
parseTillEnd p =
  parse (p <* eof) ""
