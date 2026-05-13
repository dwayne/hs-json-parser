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
    falseSpec
    trueSpec
    nullSpec


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


falseSpec :: Spec
falseSpec =
  describe "false" $ do
    it "parses \"false\"" $ do
      parseTillEnd P.false "false" `shouldParse` False
      parseTillEnd P.false "false " `shouldParse` False
      parseTillEnd P.false "false  " `shouldParse` False

      parseTillEnd P.false `shouldFailOn` "FALSE"
      parseTillEnd P.false `shouldFailOn` "False"
      parseTillEnd P.false `shouldFailOn` "falsey"


trueSpec :: Spec
trueSpec =
  describe "true" $ do
    it "parses \"true\"" $ do
      parseTillEnd P.true "true" `shouldParse` True
      parseTillEnd P.true "true " `shouldParse` True
      parseTillEnd P.true "true  " `shouldParse` True

      parseTillEnd P.true `shouldFailOn` "TRUE"
      parseTillEnd P.true `shouldFailOn` "True"
      parseTillEnd P.true `shouldFailOn` "trueish"


nullSpec :: Spec
nullSpec =
  describe "null" $ do
    it "parses \"null\"" $ do
      parseTillEnd P.null `shouldSucceedOn` "null"
      parseTillEnd P.null `shouldSucceedOn` "null "
      parseTillEnd P.null `shouldSucceedOn` "null  "

      parseTillEnd P.null `shouldFailOn` "NULL"
      parseTillEnd P.null `shouldFailOn` "Null"
      parseTillEnd P.null `shouldFailOn` "nullish"


-- Helpers


parseTillEnd :: P.Parser a -> Text -> Either P.Error a
parseTillEnd p =
  parse (p <* eof) ""
