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
  describe "ws" $ do
    it "zero" $ do
      parseTillEnd P.ws `shouldSucceedOn` ""

    it "one" $ do
      parseTillEnd P.ws `shouldSucceedOn` " "
      parseTillEnd P.ws `shouldSucceedOn` "\t"
      parseTillEnd P.ws `shouldSucceedOn` "\n"
      parseTillEnd P.ws `shouldSucceedOn` "\r"

    it "more than one" $ do
      parseTillEnd P.ws `shouldSucceedOn` " \n\r\t\t\r\n "


-- Helpers


parseTillEnd :: P.Parser a -> Text -> Either P.Error a
parseTillEnd p =
  parse (p <* eof) ""
