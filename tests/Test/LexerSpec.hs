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
    numberSpec
    signedNaturalSpec
    fractionalPartSpec
    exponentPartSpec


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


numberSpec :: Spec
numberSpec =
  describe "number" $ do
    it "parses a number" $ do
      -- integer, no fraction, no exponent
      parseTillEnd L.number "0" `shouldParse` L.Number (L.SignedNatural L.Plus "0") Nothing Nothing
      parseTillEnd L.number "-0" `shouldParse` L.Number (L.SignedNatural L.Minus "0") Nothing Nothing
      parseTillEnd L.number "5" `shouldParse` L.Number (L.SignedNatural L.Plus "5") Nothing Nothing
      parseTillEnd L.number "-5" `shouldParse` L.Number (L.SignedNatural L.Minus "5") Nothing Nothing

      -- integer, fraction, no exponent
      parseTillEnd L.number "0.5" `shouldParse` L.Number (L.SignedNatural L.Plus "0") (Just $ L.FractionalPart "5" 1) Nothing
      parseTillEnd L.number "10.25" `shouldParse` L.Number (L.SignedNatural L.Plus "10") (Just $ L.FractionalPart "25" 2) Nothing
      parseTillEnd L.number "-3.0125" `shouldParse` L.Number (L.SignedNatural L.Minus "3") (Just $ L.FractionalPart "0125" 4) Nothing

      -- integer, no fraction, exponent
      parseTillEnd L.number "2E8" `shouldParse` L.Number (L.SignedNatural L.Plus "2") Nothing (Just $ L.ExponentPart L.Plus "8")
      parseTillEnd L.number "2E+8" `shouldParse` L.Number (L.SignedNatural L.Plus "2") Nothing (Just $ L.ExponentPart L.Plus "8")
      parseTillEnd L.number "2E-8" `shouldParse` L.Number (L.SignedNatural L.Plus "2") Nothing (Just $ L.ExponentPart L.Minus "8")
      parseTillEnd L.number "2e8" `shouldParse` L.Number (L.SignedNatural L.Plus "2") Nothing (Just $ L.ExponentPart L.Plus "8")
      parseTillEnd L.number "2e+8" `shouldParse` L.Number (L.SignedNatural L.Plus "2") Nothing (Just $ L.ExponentPart L.Plus "8")
      parseTillEnd L.number "2e-8" `shouldParse` L.Number (L.SignedNatural L.Plus "2") Nothing (Just $ L.ExponentPart L.Minus "8")
      parseTillEnd L.number "-123e-0456" `shouldParse` L.Number (L.SignedNatural L.Minus "123") Nothing (Just $ L.ExponentPart L.Minus "0456")

      -- integer, fraction, exponent
      parseTillEnd L.number "1.2e3" `shouldParse` L.Number (L.SignedNatural L.Plus "1") (Just $ L.FractionalPart "2" 1) (Just $ L.ExponentPart L.Plus "3")
      parseTillEnd L.number "-1.2E-3" `shouldParse` L.Number (L.SignedNatural L.Minus "1") (Just $ L.FractionalPart "2" 1) (Just $ L.ExponentPart L.Minus "3")

      -- expected failures
      parseTillEnd L.number `shouldFailOn` ""
      parseTillEnd L.number `shouldFailOn` "-"
      parseTillEnd L.number `shouldFailOn` "1."
      parseTillEnd L.number `shouldFailOn` ".1"


signedNaturalSpec :: Spec
signedNaturalSpec =
  describe "signedNatural" $ do
    it "parses a signed natural number" $ do
      -- zero
      parseTillEnd L.signedNatural "0" `shouldParse` L.SignedNatural L.Plus "0"
      parseTillEnd L.signedNatural "-0" `shouldParse` L.SignedNatural L.Minus "0"

      -- non-zero digit
      parseTillEnd L.signedNatural "1" `shouldParse` L.SignedNatural L.Plus "1"
      parseTillEnd L.signedNatural "2" `shouldParse` L.SignedNatural L.Plus "2"
      parseTillEnd L.signedNatural "3" `shouldParse` L.SignedNatural L.Plus "3"
      parseTillEnd L.signedNatural "4" `shouldParse` L.SignedNatural L.Plus "4"
      parseTillEnd L.signedNatural "5" `shouldParse` L.SignedNatural L.Plus "5"
      parseTillEnd L.signedNatural "6" `shouldParse` L.SignedNatural L.Plus "6"
      parseTillEnd L.signedNatural "7" `shouldParse` L.SignedNatural L.Plus "7"
      parseTillEnd L.signedNatural "8" `shouldParse` L.SignedNatural L.Plus "8"
      parseTillEnd L.signedNatural "9" `shouldParse` L.SignedNatural L.Plus "9"

      -- more than one digit
      parseTillEnd L.signedNatural "10" `shouldParse` L.SignedNatural L.Plus "10"
      parseTillEnd L.signedNatural "1234567890" `shouldParse` L.SignedNatural L.Plus "1234567890"

      -- negative numbers
      parseTillEnd L.signedNatural "-1" `shouldParse` L.SignedNatural L.Minus "1"
      parseTillEnd L.signedNatural "-10" `shouldParse` L.SignedNatural L.Minus "10"

      -- expected failures
      parseTillEnd L.signedNatural `shouldFailOn` ""
      parseTillEnd L.signedNatural `shouldFailOn` "-"
      parseTillEnd L.signedNatural `shouldFailOn` "01"


fractionalPartSpec :: Spec
fractionalPartSpec =
  describe "fractionalPart" $ do
    it "parses the optional fractional part of a number" $ do
      -- one or more digits after the decimal point
      parseTillEnd L.fractionalPart ".5" `shouldParse` L.FractionalPart "5" 1
      parseTillEnd L.fractionalPart ".25" `shouldParse` L.FractionalPart "25" 2
      parseTillEnd L.fractionalPart ".125" `shouldParse` L.FractionalPart "125" 3
      parseTillEnd L.fractionalPart ".000125" `shouldParse` L.FractionalPart "000125" 6

      -- zeros
      parseTillEnd L.fractionalPart ".0" `shouldParse` L.FractionalPart "0" 1
      parseTillEnd L.fractionalPart ".00" `shouldParse` L.FractionalPart "00" 2
      parseTillEnd L.fractionalPart ".0000000000" `shouldParse` L.FractionalPart "0000000000" 10

      -- expected failures
      parseTillEnd L.fractionalPart `shouldFailOn` ""
      parseTillEnd L.fractionalPart `shouldFailOn` "."


exponentPartSpec :: Spec
exponentPartSpec =
  describe "exponentPart" $ do
    it "parses the exponent part of a number" $ do
      -- with E
      parseTillEnd L.exponentPart "E12" `shouldParse` L.ExponentPart L.Plus "12"
      parseTillEnd L.exponentPart "E+12" `shouldParse` L.ExponentPart L.Plus "12"
      parseTillEnd L.exponentPart "E-12" `shouldParse` L.ExponentPart L.Minus "12"

      -- with e
      parseTillEnd L.exponentPart "e12" `shouldParse` L.ExponentPart L.Plus "12"
      parseTillEnd L.exponentPart "e+12" `shouldParse` L.ExponentPart L.Plus "12"
      parseTillEnd L.exponentPart "e-12" `shouldParse` L.ExponentPart L.Minus "12"

      -- zeros
      parseTillEnd L.exponentPart "E0" `shouldParse` L.ExponentPart L.Plus "0"
      parseTillEnd L.exponentPart "E+00" `shouldParse` L.ExponentPart L.Plus "00"
      parseTillEnd L.exponentPart "E-000" `shouldParse` L.ExponentPart L.Minus "000"


-- Helpers


parseTillEnd :: L.Parser a -> Text -> Either L.Error a
parseTillEnd p =
  parse (p <* eof) ""
