{-# LANGUAGE OverloadedStrings #-}

module Test.LexerSpec (spec) where

import qualified Lexer as L

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec (parse)


spec :: Spec
spec =
  describe "Lexer" $ do
    wsSpec
    signSpec


wsSpec :: Spec
wsSpec =
  describe "ws" $ do
    it "parses zero or more whitespace characters" $ do
      -- zero
      parse L.ws "" `shouldSucceedOn` ""

      -- one
      parse L.ws "" `shouldSucceedOn` " "
      parse L.ws "" `shouldSucceedOn` "\n"
      parse L.ws "" `shouldSucceedOn` "\r"
      parse L.ws "" `shouldSucceedOn` "\t"

      -- more than one
      parse L.ws "" `shouldSucceedOn` " \n\r\t\t\r\n "


signSpec :: Spec
signSpec =
  describe "sign" $ do
    it "parses an optional plus or minus sign" $ do
      -- plus sign
      parse L.sign "" "+" `shouldParse` Just L.Plus

      -- minus sign
      parse L.sign "" "-" `shouldParse` Just L.Minus

      -- no sign
      parse L.sign "" "" `shouldParse` Nothing
      parse L.sign "" "a" `shouldParse` Nothing
      parse L.sign "" "1" `shouldParse` Nothing
