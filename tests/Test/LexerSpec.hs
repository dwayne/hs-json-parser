{-# LANGUAGE OverloadedStrings #-}

module Test.LexerSpec (spec) where

import qualified Lexer as L

import Data.Text (Text)
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec (parse, eof)


spec :: Spec
spec =
  describe "Lexer" $ do
    wsSpec
    signSpec
    signedNaturalSpec
    fractionSpec


wsSpec :: Spec
wsSpec =
  describe "ws" $ do
    it "parses zero or more whitespace characters" $ do
      -- zero
      parseTillEnd L.ws `shouldSucceedOn` ""

      -- one
      parseTillEnd L.ws `shouldSucceedOn` " "
      parseTillEnd L.ws `shouldSucceedOn` "\n"
      parseTillEnd L.ws `shouldSucceedOn` "\r"
      parseTillEnd L.ws `shouldSucceedOn` "\t"

      -- more than one
      parseTillEnd L.ws `shouldSucceedOn` " \n\r\t\t\r\n "


signSpec :: Spec
signSpec =
  describe "sign" $ do
    it "parses an optional plus or minus sign" $ do
      -- plus sign
      parseTillEnd L.sign "+" `shouldParse` Just L.Plus

      -- minus sign
      parseTillEnd L.sign "-" `shouldParse` Just L.Minus

      -- no sign
      parseTillEnd L.sign "" `shouldParse` Nothing

      -- expected failures
      parseTillEnd L.sign `shouldFailOn` "1"


signedNaturalSpec :: Spec
signedNaturalSpec =
  describe "signedNatural" $ do
    it "parses a signed natural number" $ do
      -- zero
      parseTillEnd L.signedNatural "0" `shouldParse` L.SignedNatural L.Plus 0
      parseTillEnd L.signedNatural "-0" `shouldParse` L.SignedNatural L.Minus 0

      -- non-zero digit
      parseTillEnd L.signedNatural "1" `shouldParse` L.SignedNatural L.Plus 1
      parseTillEnd L.signedNatural "2" `shouldParse` L.SignedNatural L.Plus 2
      parseTillEnd L.signedNatural "3" `shouldParse` L.SignedNatural L.Plus 3
      parseTillEnd L.signedNatural "4" `shouldParse` L.SignedNatural L.Plus 4
      parseTillEnd L.signedNatural "5" `shouldParse` L.SignedNatural L.Plus 5
      parseTillEnd L.signedNatural "6" `shouldParse` L.SignedNatural L.Plus 6
      parseTillEnd L.signedNatural "7" `shouldParse` L.SignedNatural L.Plus 7
      parseTillEnd L.signedNatural "8" `shouldParse` L.SignedNatural L.Plus 8
      parseTillEnd L.signedNatural "9" `shouldParse` L.SignedNatural L.Plus 9

      -- more than one digit
      parseTillEnd L.signedNatural "10" `shouldParse` L.SignedNatural L.Plus 10
      parseTillEnd L.signedNatural "1234567890" `shouldParse` L.SignedNatural L.Plus 1234567890

      -- negative numbers
      parseTillEnd L.signedNatural "-1" `shouldParse` L.SignedNatural L.Minus 1
      parseTillEnd L.signedNatural "-10" `shouldParse` L.SignedNatural L.Minus 10

      -- expected failures
      parseTillEnd L.signedNatural `shouldFailOn` ""
      parseTillEnd L.signedNatural `shouldFailOn` "-"
      parseTillEnd L.signedNatural `shouldFailOn` "01"


fractionSpec :: Spec
fractionSpec =
  describe "fraction" $ do
    it "parses the optional fractional part of a number" $ do
      -- no fractional part
      parseTillEnd L.fraction "" `shouldParse` Nothing

      -- one or more digits after the decimal point
      parseTillEnd L.fraction ".5" `shouldParse` Just (L.FractionalPart 5 1)
      parseTillEnd L.fraction ".25" `shouldParse` Just (L.FractionalPart 25 2)
      parseTillEnd L.fraction ".125" `shouldParse` Just (L.FractionalPart 125 3)
      parseTillEnd L.fraction ".000125" `shouldParse` Just (L.FractionalPart 125 6)

      -- zeros
      parseTillEnd L.fraction ".0" `shouldParse` Just (L.FractionalPart 0 0)
      parseTillEnd L.fraction ".00" `shouldParse` Just (L.FractionalPart 0 0)
      parseTillEnd L.fraction ".0000000000" `shouldParse` Just (L.FractionalPart 0 0)

      -- expected failures
      parseTillEnd L.signedNatural `shouldFailOn` "."


-- Helpers


parseTillEnd :: L.Parser a -> Text -> Either L.Error a
parseTillEnd p =
  parse (p <* eof) ""
