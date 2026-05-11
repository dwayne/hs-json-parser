{-# LANGUAGE OverloadedStrings #-}

module Test.Json.ParserSpec (spec) where

import qualified Json.Parser as P

import Data.Text (Text)
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec (parse, eof)


spec :: Spec
spec =
  describe "Parser" $ do
    wsSpec
    nullSpec
    booleanSpec
    signedNaturalSpec
    fractionalPartSpec
    exponentPartSpec
    numberSpec
    stringSpec
    arraySpec
    objectSpec
    jsonSpec


wsSpec :: Spec
wsSpec =
  describe "ws" $ do
    it "parses zero or more whitespace characters" $ do
      -- zero
      parseTillEnd P.ws `shouldSucceedOn` ""

      -- one
      parseTillEnd P.ws `shouldSucceedOn` " "
      parseTillEnd P.ws `shouldSucceedOn` "\n"
      parseTillEnd P.ws `shouldSucceedOn` "\r"
      parseTillEnd P.ws `shouldSucceedOn` "\t"

      -- more than one
      parseTillEnd P.ws `shouldSucceedOn` " \n\r\t\t\r\n "


nullSpec :: Spec
nullSpec =
  describe "null" $ do
    it "parses \"null\"" $ do
      parseTillEnd P.null "null" `shouldParse` ()

      -- expected failures
      parseTillEnd P.null `shouldFailOn` "NULL"
      parseTillEnd P.null `shouldFailOn` "Null"
      parseTillEnd P.null `shouldFailOn` "nullish"


booleanSpec :: Spec
booleanSpec =
  describe "boolean" $ do
    it "parses \"true\" or \"false\"" $ do
      parseTillEnd P.boolean "true" `shouldParse` True
      parseTillEnd P.boolean "false" `shouldParse` False

      -- expected failures
      parseTillEnd P.boolean `shouldFailOn` "TRUE"
      parseTillEnd P.boolean `shouldFailOn` "True"
      parseTillEnd P.boolean `shouldFailOn` "trueish"
      parseTillEnd P.boolean `shouldFailOn` "FALSE"
      parseTillEnd P.boolean `shouldFailOn` "False"
      parseTillEnd P.boolean `shouldFailOn` "falsey"


signedNaturalSpec :: Spec
signedNaturalSpec =
  describe "signedNatural" $ do
    it "parses a signed natural number" $ do
      -- zero
      parseTillEnd P.signedNatural "0" `shouldParse` P.SignedNatural P.Plus "0"
      parseTillEnd P.signedNatural "-0" `shouldParse` P.SignedNatural P.Minus "0"

      -- non-zero digit
      parseTillEnd P.signedNatural "1" `shouldParse` P.SignedNatural P.Plus "1"
      parseTillEnd P.signedNatural "2" `shouldParse` P.SignedNatural P.Plus "2"
      parseTillEnd P.signedNatural "3" `shouldParse` P.SignedNatural P.Plus "3"
      parseTillEnd P.signedNatural "4" `shouldParse` P.SignedNatural P.Plus "4"
      parseTillEnd P.signedNatural "5" `shouldParse` P.SignedNatural P.Plus "5"
      parseTillEnd P.signedNatural "6" `shouldParse` P.SignedNatural P.Plus "6"
      parseTillEnd P.signedNatural "7" `shouldParse` P.SignedNatural P.Plus "7"
      parseTillEnd P.signedNatural "8" `shouldParse` P.SignedNatural P.Plus "8"
      parseTillEnd P.signedNatural "9" `shouldParse` P.SignedNatural P.Plus "9"

      -- more than one digit
      parseTillEnd P.signedNatural "10" `shouldParse` P.SignedNatural P.Plus "10"
      parseTillEnd P.signedNatural "1234567890" `shouldParse` P.SignedNatural P.Plus "1234567890"

      -- negative numbers
      parseTillEnd P.signedNatural "-1" `shouldParse` P.SignedNatural P.Minus "1"
      parseTillEnd P.signedNatural "-10" `shouldParse` P.SignedNatural P.Minus "10"

      -- expected failures
      parseTillEnd P.signedNatural `shouldFailOn` ""
      parseTillEnd P.signedNatural `shouldFailOn` "-"
      parseTillEnd P.signedNatural `shouldFailOn` "01"


fractionalPartSpec :: Spec
fractionalPartSpec =
  describe "fractionalPart" $ do
    it "parses the optional fractional part of a number" $ do
      -- one or more digits after the decimal point
      parseTillEnd P.fractionalPart ".5" `shouldParse` P.FractionalPart "5"
      parseTillEnd P.fractionalPart ".25" `shouldParse` P.FractionalPart "25"
      parseTillEnd P.fractionalPart ".125" `shouldParse` P.FractionalPart "125"
      parseTillEnd P.fractionalPart ".000125" `shouldParse` P.FractionalPart "000125"

      -- zeros
      parseTillEnd P.fractionalPart ".0" `shouldParse` P.FractionalPart "0"
      parseTillEnd P.fractionalPart ".00" `shouldParse` P.FractionalPart "00"
      parseTillEnd P.fractionalPart ".0000000000" `shouldParse` P.FractionalPart "0000000000"

      -- expected failures
      parseTillEnd P.fractionalPart `shouldFailOn` ""
      parseTillEnd P.fractionalPart `shouldFailOn` "."


exponentPartSpec :: Spec
exponentPartSpec =
  describe "exponentPart" $ do
    it "parses the exponent part of a number" $ do
      -- with E
      parseTillEnd P.exponentPart "E12" `shouldParse` P.ExponentPart P.Plus "12"
      parseTillEnd P.exponentPart "E+12" `shouldParse` P.ExponentPart P.Plus "12"
      parseTillEnd P.exponentPart "E-12" `shouldParse` P.ExponentPart P.Minus "12"

      -- with e
      parseTillEnd P.exponentPart "e12" `shouldParse` P.ExponentPart P.Plus "12"
      parseTillEnd P.exponentPart "e+12" `shouldParse` P.ExponentPart P.Plus "12"
      parseTillEnd P.exponentPart "e-12" `shouldParse` P.ExponentPart P.Minus "12"

      -- zeros
      parseTillEnd P.exponentPart "E0" `shouldParse` P.ExponentPart P.Plus "0"
      parseTillEnd P.exponentPart "E+00" `shouldParse` P.ExponentPart P.Plus "00"
      parseTillEnd P.exponentPart "E-000" `shouldParse` P.ExponentPart P.Minus "000"


numberSpec :: Spec
numberSpec =
  describe "number" $ do
    it "parses a number" $ do
      -- integer, no fraction, no exponent
      parseTillEnd P.number "0" `shouldParse` P.Number (P.SignedNatural P.Plus "0") Nothing Nothing
      parseTillEnd P.number "-0" `shouldParse` P.Number (P.SignedNatural P.Minus "0") Nothing Nothing
      parseTillEnd P.number "5" `shouldParse` P.Number (P.SignedNatural P.Plus "5") Nothing Nothing
      parseTillEnd P.number "-5" `shouldParse` P.Number (P.SignedNatural P.Minus "5") Nothing Nothing

      -- integer, fraction, no exponent
      parseTillEnd P.number "0.5" `shouldParse` P.Number (P.SignedNatural P.Plus "0") (Just $ P.FractionalPart "5") Nothing
      parseTillEnd P.number "10.25" `shouldParse` P.Number (P.SignedNatural P.Plus "10") (Just $ P.FractionalPart "25") Nothing
      parseTillEnd P.number "-3.0125" `shouldParse` P.Number (P.SignedNatural P.Minus "3") (Just $ P.FractionalPart "0125") Nothing

      -- integer, no fraction, exponent
      parseTillEnd P.number "2E8" `shouldParse` P.Number (P.SignedNatural P.Plus "2") Nothing (Just $ P.ExponentPart P.Plus "8")
      parseTillEnd P.number "2E+8" `shouldParse` P.Number (P.SignedNatural P.Plus "2") Nothing (Just $ P.ExponentPart P.Plus "8")
      parseTillEnd P.number "2E-8" `shouldParse` P.Number (P.SignedNatural P.Plus "2") Nothing (Just $ P.ExponentPart P.Minus "8")
      parseTillEnd P.number "2e8" `shouldParse` P.Number (P.SignedNatural P.Plus "2") Nothing (Just $ P.ExponentPart P.Plus "8")
      parseTillEnd P.number "2e+8" `shouldParse` P.Number (P.SignedNatural P.Plus "2") Nothing (Just $ P.ExponentPart P.Plus "8")
      parseTillEnd P.number "2e-8" `shouldParse` P.Number (P.SignedNatural P.Plus "2") Nothing (Just $ P.ExponentPart P.Minus "8")
      parseTillEnd P.number "-123e-0456" `shouldParse` P.Number (P.SignedNatural P.Minus "123") Nothing (Just $ P.ExponentPart P.Minus "0456")

      -- integer, fraction, exponent
      parseTillEnd P.number "1.2e3" `shouldParse` P.Number (P.SignedNatural P.Plus "1") (Just $ P.FractionalPart "2") (Just $ P.ExponentPart P.Plus "3")
      parseTillEnd P.number "-1.2E-3" `shouldParse` P.Number (P.SignedNatural P.Minus "1") (Just $ P.FractionalPart "2") (Just $ P.ExponentPart P.Minus "3")

      -- expected failures
      parseTillEnd P.number `shouldFailOn` ""
      parseTillEnd P.number `shouldFailOn` "-"
      parseTillEnd P.number `shouldFailOn` "1."
      parseTillEnd P.number `shouldFailOn` ".1"


stringSpec :: Spec
stringSpec =
  describe "string" $ do
    it "parses a string" $ do
      -- empty
      parseTillEnd P.string "\"\"" `shouldParse` ""

      -- regular
      parseTillEnd P.string "\"abcdef0123ABC!@#$%\"" `shouldParse` "abcdef0123ABC!@#$%"

      -- escape
      parseTillEnd P.string "\"\\\"\"" `shouldParse` "\""
      parseTillEnd P.string "\"\\\\\"" `shouldParse` "\\"
      parseTillEnd P.string "\"\\/\"" `shouldParse` "/"
      parseTillEnd P.string "\"\\b\"" `shouldParse` "\b"
      parseTillEnd P.string "\"\\f\"" `shouldParse` "\f"
      parseTillEnd P.string "\"\\n\"" `shouldParse` "\n"
      parseTillEnd P.string "\"\\r\"" `shouldParse` "\r"
      parseTillEnd P.string "\"\\t\"" `shouldParse` "\t"

      -- Unicode code points
      parseTillEnd P.string "\"\\u0022\"" `shouldParse` "\""
      parseTillEnd P.string "\"\\u005C\"" `shouldParse` "\\"
      parseTillEnd P.string "\"\\u005c\"" `shouldParse` "\\"
      parseTillEnd P.string "\"\\u002F\"" `shouldParse` "/"
      parseTillEnd P.string "\"\\u0008\"" `shouldParse` "\b"
      parseTillEnd P.string "\"\\u000C\"" `shouldParse` "\f"
      parseTillEnd P.string "\"\\u000A\"" `shouldParse` "\n"
      parseTillEnd P.string "\"\\u000D\"" `shouldParse` "\r"
      parseTillEnd P.string "\"\\u0009\"" `shouldParse` "\t"

      -- mixed
      parseTillEnd P.string "\"\\\"a\\\\b\\/c\\bd\\fe\\nf\\r\\t\\u002f\"" `shouldParse` "\"a\\b/c\bd\fe\nf\r\t/"


arraySpec :: Spec
arraySpec =
  describe "array" $ do
    it "parses an array" $ do
      -- empty
      parseTillEnd P.array "[]" `shouldParse` []
      parseTillEnd P.array "[ ]" `shouldParse` []
      parseTillEnd P.array "[  ]" `shouldParse` []

      -- singleton
      parseTillEnd P.array "[true]" `shouldParse` [ P.JsonBoolean True ]
      parseTillEnd P.array "[ true ]" `shouldParse` [ P.JsonBoolean True ]

      -- more than one
      parseTillEnd P.array "[true,false]" `shouldParse` [ P.JsonBoolean True, P.JsonBoolean False ]
      parseTillEnd P.array "[ true  ,   false    ]     " `shouldParse` [ P.JsonBoolean True, P.JsonBoolean False ]


objectSpec :: Spec
objectSpec =
  describe "object" $ do
    it "parses an object" $ do
      -- empty
      parseTillEnd P.object "{}" `shouldParse` []
      parseTillEnd P.object "{ }" `shouldParse` []
      parseTillEnd P.object "{  }" `shouldParse` []

      -- non-empty
      parseTillEnd P.object "{\"x\":5}" `shouldParse` [("x", P.JsonNumber (P.Number (P.SignedNatural P.Plus "5") Nothing Nothing))]
      parseTillEnd P.object "{ \"x\": 5 }" `shouldParse` [("x", P.JsonNumber (P.Number (P.SignedNatural P.Plus "5") Nothing Nothing))]

      parseTillEnd P.object "{ \"x\": 5, \"y\": 10 }" `shouldParse` [
          ("x", P.JsonNumber (P.Number (P.SignedNatural P.Plus "5") Nothing Nothing)),
          ("y", P.JsonNumber (P.Number (P.SignedNatural P.Plus "10") Nothing Nothing))
        ]

      -- expected failures
      parseTillEnd P.object `shouldFailOn` "{ \"x\": 5, }"
      parseTillEnd P.object `shouldFailOn` "{ \"x\": 5,, }"


jsonSpec :: Spec
jsonSpec =
  describe "json" $ do
    it "parses a JSON value" $ do
      parseTillEnd P.json "null" `shouldParse` P.JsonNull
      parseTillEnd P.json "true" `shouldParse` P.JsonBoolean True
      parseTillEnd P.json "false" `shouldParse` P.JsonBoolean False
      parseTillEnd P.json "123" `shouldParse` P.JsonNumber (P.Number (P.SignedNatural P.Plus "123") Nothing Nothing)
      parseTillEnd P.json "\"hello\"" `shouldParse` P.JsonString "hello"
      parseTillEnd P.json "[]" `shouldParse` P.JsonArray []


-- Helpers


parseTillEnd :: P.Parser a -> Text -> Either P.Error a
parseTillEnd p =
  parse (p <* eof) ""
