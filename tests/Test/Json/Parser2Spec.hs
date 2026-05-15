{-# LANGUAGE OverloadedStrings #-}

module Test.Json.Parser2Spec (spec) where

import qualified Json.Parser2 as P

import Data.Text (Text)
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec (parse, eof)


--
-- TODO:
--
-- - [ ] Improve the descriptions of the test cases (see what I did for stringSpec)
--


spec :: Spec
spec =
  describe "Parser" $ do
    oneOrMoreWhitespacesSpec
    nullSpec
    falseSpec
    trueSpec
    booleanSpec
    numberSpec
    stringSpec
    arraySpec
    objectSpec
    jsonSpec


oneOrMoreWhitespacesSpec :: Spec
oneOrMoreWhitespacesSpec =
  describe "oneOrMoreWhitespaces" $ do
    it "parses zero whitespace characters" $ do
      parseTillEnd P.oneOrMoreWhitespaces `shouldFailOn` ""

    it "one" $ do
      parseTillEnd P.oneOrMoreWhitespaces `shouldSucceedOn` " "
      parseTillEnd P.oneOrMoreWhitespaces `shouldSucceedOn` "\t"
      parseTillEnd P.oneOrMoreWhitespaces `shouldSucceedOn` "\n"
      parseTillEnd P.oneOrMoreWhitespaces `shouldSucceedOn` "\r"

    it "more than one" $ do
      parseTillEnd P.oneOrMoreWhitespaces `shouldSucceedOn` " \n\r\t\t\r\n "


nullSpec :: Spec
nullSpec =
  describe "null" $ do
    it "parses \"null\"" $ do
      parseTillEnd P.null `shouldSucceedOn` "null"

      parseTillEnd P.null `shouldFailOn` "NULL"
      parseTillEnd P.null `shouldFailOn` "Null"
      parseTillEnd P.null `shouldFailOn` "nullish"

    it "consumes trailing spaces" $ do
      parseTillEnd P.null `shouldSucceedOn` "null "


falseSpec :: Spec
falseSpec =
  describe "false" $ do
    it "parses \"false\"" $ do
      parseTillEnd P.false "false" `shouldParse` False

      parseTillEnd P.false `shouldFailOn` "FALSE"
      parseTillEnd P.false `shouldFailOn` "False"
      parseTillEnd P.false `shouldFailOn` "falsey"

    it "consumes trailing spaces" $ do
      parseTillEnd P.false "false " `shouldParse` False


trueSpec :: Spec
trueSpec =
  describe "true" $ do
    it "parses \"true\"" $ do
      parseTillEnd P.true "true" `shouldParse` True

      parseTillEnd P.true `shouldFailOn` "TRUE"
      parseTillEnd P.true `shouldFailOn` "True"
      parseTillEnd P.true `shouldFailOn` "trueish"

    it "consumes trailing spaces" $ do
      parseTillEnd P.true "true " `shouldParse` True


booleanSpec :: Spec
booleanSpec =
  describe "boolean" $ do
    it "parses \"true\" or \"false\"" $ do
      parseTillEnd P.boolean "true" `shouldParse` True
      parseTillEnd P.boolean "false" `shouldParse` False

    it "consumes trailing spaces" $ do
      parseTillEnd P.boolean "true " `shouldParse` True
      parseTillEnd P.boolean "false " `shouldParse` False


numberSpec :: Spec
numberSpec =
  describe "number" $ do
    it "zero" $ do
      parseTillEnd P.number "0" `shouldParse` P.Number P.Plus "0" Nothing Nothing

    it "negative zero" $ do
      parseTillEnd P.number "-0" `shouldParse` P.Number P.Minus "0" Nothing Nothing

    it "positive integer" $ do
      parseTillEnd P.number "1" `shouldParse` P.Number P.Plus "1" Nothing Nothing
      parseTillEnd P.number "2" `shouldParse` P.Number P.Plus "2" Nothing Nothing
      parseTillEnd P.number "3" `shouldParse` P.Number P.Plus "3" Nothing Nothing
      parseTillEnd P.number "4" `shouldParse` P.Number P.Plus "4" Nothing Nothing
      parseTillEnd P.number "5" `shouldParse` P.Number P.Plus "5" Nothing Nothing
      parseTillEnd P.number "6" `shouldParse` P.Number P.Plus "6" Nothing Nothing
      parseTillEnd P.number "7" `shouldParse` P.Number P.Plus "7" Nothing Nothing
      parseTillEnd P.number "8" `shouldParse` P.Number P.Plus "8" Nothing Nothing
      parseTillEnd P.number "9" `shouldParse` P.Number P.Plus "9" Nothing Nothing
      parseTillEnd P.number "123456789" `shouldParse` P.Number P.Plus "123456789" Nothing Nothing

    it "negative integer" $ do
      parseTillEnd P.number "-1" `shouldParse` P.Number P.Minus "1" Nothing Nothing
      parseTillEnd P.number "-2" `shouldParse` P.Number P.Minus "2" Nothing Nothing
      parseTillEnd P.number "-3" `shouldParse` P.Number P.Minus "3" Nothing Nothing
      parseTillEnd P.number "-4" `shouldParse` P.Number P.Minus "4" Nothing Nothing
      parseTillEnd P.number "-5" `shouldParse` P.Number P.Minus "5" Nothing Nothing
      parseTillEnd P.number "-6" `shouldParse` P.Number P.Minus "6" Nothing Nothing
      parseTillEnd P.number "-7" `shouldParse` P.Number P.Minus "7" Nothing Nothing
      parseTillEnd P.number "-8" `shouldParse` P.Number P.Minus "8" Nothing Nothing
      parseTillEnd P.number "-9" `shouldParse` P.Number P.Minus "9" Nothing Nothing
      parseTillEnd P.number "-123456789" `shouldParse` P.Number P.Minus "123456789" Nothing Nothing

    it "fractional part" $ do
      parseTillEnd P.number "123.456" `shouldParse` P.Number P.Plus "123" (Just "456") Nothing
      parseTillEnd P.number "-123.456" `shouldParse` P.Number P.Minus "123" (Just "456") Nothing
      parseTillEnd P.number "0.5" `shouldParse` P.Number P.Plus "0" (Just "5") Nothing
      parseTillEnd P.number "-0.5" `shouldParse` P.Number P.Minus "0" (Just "5") Nothing
      parseTillEnd P.number "0.005" `shouldParse` P.Number P.Plus "0" (Just "005") Nothing
      parseTillEnd P.number "0.000" `shouldParse` P.Number P.Plus "0" (Just "000") Nothing
      parseTillEnd P.number "-0.000" `shouldParse` P.Number P.Minus "0" (Just "000") Nothing

    it "exponent part" $ do
      parseTillEnd P.number "123.456E78" `shouldParse` P.Number P.Plus "123" (Just "456") (Just (P.Plus, "78"))
      parseTillEnd P.number "123.456E+78" `shouldParse` P.Number P.Plus "123" (Just "456") (Just (P.Plus, "78"))
      parseTillEnd P.number "123.456E-78" `shouldParse` P.Number P.Plus "123" (Just "456") (Just (P.Minus, "78"))

      parseTillEnd P.number "123.456e78" `shouldParse` P.Number P.Plus "123" (Just "456") (Just (P.Plus, "78"))
      parseTillEnd P.number "123.456e+78" `shouldParse` P.Number P.Plus "123" (Just "456") (Just (P.Plus, "78"))
      parseTillEnd P.number "123.456e-78" `shouldParse` P.Number P.Plus "123" (Just "456") (Just (P.Minus, "78"))

      parseTillEnd P.number "123.456E0" `shouldParse` P.Number P.Plus "123" (Just "456") (Just (P.Plus, "0"))
      parseTillEnd P.number "123.456E00" `shouldParse` P.Number P.Plus "123" (Just "456") (Just (P.Plus, "00"))
      parseTillEnd P.number "123.456E-0" `shouldParse` P.Number P.Plus "123" (Just "456") (Just (P.Minus, "0"))

    it "consumes trailing spaces" $ do
      parseTillEnd P.number "0 " `shouldParse` P.Number P.Plus "0" Nothing Nothing


stringSpec :: Spec
stringSpec =
  describe "string" $ do
    it "parses the empty string" $ do
      parseTillEnd P.string "\"\"" `shouldParse` ""

    it "parses one or more spaces" $ do
      parseTillEnd P.string "\" \"" `shouldParse` " "
      parseTillEnd P.string "\"  \"" `shouldParse` "  "
      parseTillEnd P.string "\"   \"" `shouldParse` "   "

    it "parses unescaped characters" $ do
      parseTillEnd P.string "\"abcdef0123ABC!@#$%\"" `shouldParse` "abcdef0123ABC!@#$%"

    it "parses escaped characters" $ do
      parseTillEnd P.string "\"\\\"\"" `shouldParse` "\""
      parseTillEnd P.string "\"\\\\\"" `shouldParse` "\\"
      parseTillEnd P.string "\"\\/\"" `shouldParse` "/"
      parseTillEnd P.string "\"\\b\"" `shouldParse` "\b"
      parseTillEnd P.string "\"\\f\"" `shouldParse` "\f"
      parseTillEnd P.string "\"\\n\"" `shouldParse` "\n"
      parseTillEnd P.string "\"\\r\"" `shouldParse` "\r"
      parseTillEnd P.string "\"\\t\"" `shouldParse` "\t"

    it "parses unicode escapes" $ do
      parseTillEnd P.string "\"\\u0022\"" `shouldParse` "\""
      parseTillEnd P.string "\"\\u005C\"" `shouldParse` "\\"
      parseTillEnd P.string "\"\\u005c\"" `shouldParse` "\\"
      parseTillEnd P.string "\"\\u002F\"" `shouldParse` "/"
      parseTillEnd P.string "\"\\u0008\"" `shouldParse` "\b"
      parseTillEnd P.string "\"\\u000C\"" `shouldParse` "\f"
      parseTillEnd P.string "\"\\u000A\"" `shouldParse` "\n"
      parseTillEnd P.string "\"\\u000D\"" `shouldParse` "\r"
      parseTillEnd P.string "\"\\u0009\"" `shouldParse` "\t"

    it "parses blank strings" $ do
      parseTillEnd P.string "\"\\n\"" `shouldParse` "\n"
      parseTillEnd P.string "\"\\t\"" `shouldParse` "\t"
      parseTillEnd P.string "\"  \\n \\t\\t  \\n  \"" `shouldParse` "  \n \t\t  \n  "

    it "parses surrogate code points" $ do
      parseTillEnd P.string "\"\\uD800\"" `shouldParse` "\xD800"
      parseTillEnd P.string "\"\\uDFFF\"" `shouldParse` "\xDFFF"
      parseTillEnd P.string "\"\\uDEAD\"" `shouldParse` "\xDEAD"

    it "consumes trailing spaces" $ do
      parseTillEnd P.string "\"\" " `shouldParse` ""


arraySpec :: Spec
arraySpec =
  describe "array" $ do
    it "parses the empty array" $ do
      parseTillEnd P.array "[]" `shouldParse` []
      parseTillEnd P.array "[ ]" `shouldParse` []

    it "parses non-empty arrays" $ do
      parseTillEnd P.array "[null]" `shouldParse` [ P.JsonNull ]
      parseTillEnd P.array "[ null ]" `shouldParse` [ P.JsonNull ]
      parseTillEnd P.array "[ false, true ]" `shouldParse` [ P.JsonBoolean False, P.JsonBoolean True ]
      parseTillEnd P.array "[ 1, 2, 3, 4, 5 ]" `shouldParse`
        [ P.JsonNumber (P.Number P.Plus "1" Nothing Nothing)
        , P.JsonNumber (P.Number P.Plus "2" Nothing Nothing)
        , P.JsonNumber (P.Number P.Plus "3" Nothing Nothing)
        , P.JsonNumber (P.Number P.Plus "4" Nothing Nothing)
        , P.JsonNumber (P.Number P.Plus "5" Nothing Nothing)
        ]
      parseTillEnd P.array "[]" `shouldParse` []

    it "parses nested arrays" $ do
      parseTillEnd P.array "[[[]]]" `shouldParse` [ P.JsonArray [ P.JsonArray [] ] ]

    it "parses heterogeneous arrays" $ do
      parseTillEnd P.array "[ null, false, true, 1, [], {} ]" `shouldParse`
        [ P.JsonNull
        , P.JsonBoolean False
        , P.JsonBoolean True
        , P.JsonNumber (P.Number P.Plus "1" Nothing Nothing)
        , P.JsonArray []
        , P.JsonObject []
        ]

    it "consumes trailing spaces" $ do
      parseTillEnd P.array "[] " `shouldParse` []


objectSpec :: Spec
objectSpec =
  describe "object" $ do
    it "parses the empty object" $ do
      parseTillEnd P.object "{}" `shouldParse` []
      parseTillEnd P.object "{ }" `shouldParse` []

    it "parses non-empty objects" $ do
      parseTillEnd P.object "{\"a\":null}" `shouldParse` [( "a", P.JsonNull )]
      parseTillEnd P.object "{ \"a\": null }" `shouldParse` [( "a", P.JsonNull )]
      parseTillEnd P.object
        "{ \"b\": false \
        \, \"c\": true  \
        \, \"d\": 5     \
        \, \"e\": []    \
        \}              "
        `shouldParse`
        [ ( "b", P.JsonBoolean False )
        , ( "c", P.JsonBoolean True )
        , ( "d", P.JsonNumber (P.Number P.Plus "5" Nothing Nothing) )
        , ( "e", P.JsonArray [] )
        ]

    it "parses nested objects" $ do
      parseTillEnd P.object "{ \"x\": { \"y\": {} } }" `shouldParse` [( "x", P.JsonObject [( "y", P.JsonObject [] )] )]

    it "consumes trailing spaces" $ do
      parseTillEnd P.object "{} " `shouldParse` []


jsonSpec :: Spec
jsonSpec =
  describe "json" $ do
    it "parses JSON" $ do
      parseTillEnd P.json "null" `shouldParse` P.JsonNull
      parseTillEnd P.json "false" `shouldParse` P.JsonBoolean False
      parseTillEnd P.json "true" `shouldParse` P.JsonBoolean True
      parseTillEnd P.json "123" `shouldParse` P.JsonNumber (P.Number P.Plus "123" Nothing Nothing)
      parseTillEnd P.json "\"Hello\"" `shouldParse` P.JsonString "Hello"
      parseTillEnd P.json "[[], null]" `shouldParse` P.JsonArray [ P.JsonArray [], P.JsonNull ]
      parseTillEnd P.json "{ \"a\": null }" `shouldParse` P.JsonObject [( "a", P.JsonNull )]

    it "consumes trailing spaces" $ do
      parseTillEnd P.json "null " `shouldParse` P.JsonNull


-- Helpers


parseTillEnd :: P.Parser a -> Text -> Either P.Error a
parseTillEnd p =
  parse (p <* eof) ""
