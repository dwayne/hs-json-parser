{-# LANGUAGE OverloadedStrings #-}

module Test.Internal.Text.ParserSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import qualified Internal.Text.Parser as P

import Data.List (isInfixOf, isPrefixOf, isSuffixOf, sortOn)
import Data.Text (Text)
import Data.Text.Encoding.Error (UnicodeException)
import Internal.Data.Json
import System.Directory (listDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec (parse, eof)


spec :: Spec
spec =
  describe "Text.Parser" $ do
    oneOrMoreWhitespacesSpec
    nullSpec
    falseSpec
    trueSpec
    booleanSpec
    numberSpec
    stringSpec
    arraySpec
    objectSpec
    valueSpec
    nstJsonTestSuiteSpec
    errorMessagesSpec


oneOrMoreWhitespacesSpec :: Spec
oneOrMoreWhitespacesSpec =
  describe "oneOrMoreWhitespaces" $ do
    it "parses zero whitespace characters" $ do
      parseTillEnd P.oneOrMoreWhitespaces `shouldFailOn` ""

    it "parses one whitespace character" $ do
      parseTillEnd P.oneOrMoreWhitespaces `shouldSucceedOn` " "
      parseTillEnd P.oneOrMoreWhitespaces `shouldSucceedOn` "\t"
      parseTillEnd P.oneOrMoreWhitespaces `shouldSucceedOn` "\n"
      parseTillEnd P.oneOrMoreWhitespaces `shouldSucceedOn` "\r"

    it "parses more than one whitespace character" $ do
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
    it "parses zero" $ do
      parseTillEnd P.number "0" `shouldParse` Number Plus "0" Nothing Nothing

    it "parses negative zero" $ do
      parseTillEnd P.number "-0" `shouldParse` Number Minus "0" Nothing Nothing

    it "parses positive integers" $ do
      parseTillEnd P.number "1" `shouldParse` Number Plus "1" Nothing Nothing
      parseTillEnd P.number "2" `shouldParse` Number Plus "2" Nothing Nothing
      parseTillEnd P.number "3" `shouldParse` Number Plus "3" Nothing Nothing
      parseTillEnd P.number "4" `shouldParse` Number Plus "4" Nothing Nothing
      parseTillEnd P.number "5" `shouldParse` Number Plus "5" Nothing Nothing
      parseTillEnd P.number "6" `shouldParse` Number Plus "6" Nothing Nothing
      parseTillEnd P.number "7" `shouldParse` Number Plus "7" Nothing Nothing
      parseTillEnd P.number "8" `shouldParse` Number Plus "8" Nothing Nothing
      parseTillEnd P.number "9" `shouldParse` Number Plus "9" Nothing Nothing
      parseTillEnd P.number "123456789" `shouldParse` Number Plus "123456789" Nothing Nothing

    it "parses negative integers" $ do
      parseTillEnd P.number "-1" `shouldParse` Number Minus "1" Nothing Nothing
      parseTillEnd P.number "-2" `shouldParse` Number Minus "2" Nothing Nothing
      parseTillEnd P.number "-3" `shouldParse` Number Minus "3" Nothing Nothing
      parseTillEnd P.number "-4" `shouldParse` Number Minus "4" Nothing Nothing
      parseTillEnd P.number "-5" `shouldParse` Number Minus "5" Nothing Nothing
      parseTillEnd P.number "-6" `shouldParse` Number Minus "6" Nothing Nothing
      parseTillEnd P.number "-7" `shouldParse` Number Minus "7" Nothing Nothing
      parseTillEnd P.number "-8" `shouldParse` Number Minus "8" Nothing Nothing
      parseTillEnd P.number "-9" `shouldParse` Number Minus "9" Nothing Nothing
      parseTillEnd P.number "-123456789" `shouldParse` Number Minus "123456789" Nothing Nothing

    it "parses numbers with a fractional part" $ do
      parseTillEnd P.number "123.456" `shouldParse` Number Plus "123" (Just "456") Nothing
      parseTillEnd P.number "-123.456" `shouldParse` Number Minus "123" (Just "456") Nothing
      parseTillEnd P.number "0.5" `shouldParse` Number Plus "0" (Just "5") Nothing
      parseTillEnd P.number "-0.5" `shouldParse` Number Minus "0" (Just "5") Nothing
      parseTillEnd P.number "0.005" `shouldParse` Number Plus "0" (Just "005") Nothing
      parseTillEnd P.number "0.000" `shouldParse` Number Plus "0" (Just "000") Nothing
      parseTillEnd P.number "-0.000" `shouldParse` Number Minus "0" (Just "000") Nothing

    it "parses numbers with an exponent part" $ do
      parseTillEnd P.number "123.456E78" `shouldParse` Number Plus "123" (Just "456") (Just (Plus, "78"))
      parseTillEnd P.number "123.456E+78" `shouldParse` Number Plus "123" (Just "456") (Just (Plus, "78"))
      parseTillEnd P.number "123.456E-78" `shouldParse` Number Plus "123" (Just "456") (Just (Minus, "78"))

      parseTillEnd P.number "123.456e78" `shouldParse` Number Plus "123" (Just "456") (Just (Plus, "78"))
      parseTillEnd P.number "123.456e+78" `shouldParse` Number Plus "123" (Just "456") (Just (Plus, "78"))
      parseTillEnd P.number "123.456e-78" `shouldParse` Number Plus "123" (Just "456") (Just (Minus, "78"))

      parseTillEnd P.number "123.456E0" `shouldParse` Number Plus "123" (Just "456") (Just (Plus, "0"))
      parseTillEnd P.number "123.456E00" `shouldParse` Number Plus "123" (Just "456") (Just (Plus, "00"))
      parseTillEnd P.number "123.456E-0" `shouldParse` Number Plus "123" (Just "456") (Just (Minus, "0"))

    it "consumes trailing spaces" $ do
      parseTillEnd P.number "0 " `shouldParse` Number Plus "0" Nothing Nothing


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
      parseTillEnd P.array "[null]" `shouldParse` [ JsonNull ]
      parseTillEnd P.array "[ null ]" `shouldParse` [ JsonNull ]
      parseTillEnd P.array "[ false, true ]" `shouldParse` [ JsonBoolean False, JsonBoolean True ]
      parseTillEnd P.array "[ 1, 2, 3, 4, 5 ]" `shouldParse`
        [ JsonNumber (Number Plus "1" Nothing Nothing)
        , JsonNumber (Number Plus "2" Nothing Nothing)
        , JsonNumber (Number Plus "3" Nothing Nothing)
        , JsonNumber (Number Plus "4" Nothing Nothing)
        , JsonNumber (Number Plus "5" Nothing Nothing)
        ]
      parseTillEnd P.array "[]" `shouldParse` []

    it "parses nested arrays" $ do
      parseTillEnd P.array "[[[]]]" `shouldParse` [ JsonArray [ JsonArray [] ] ]

    it "parses heterogeneous arrays" $ do
      parseTillEnd P.array "[ null, false, true, 1, [], {} ]" `shouldParse`
        [ JsonNull
        , JsonBoolean False
        , JsonBoolean True
        , JsonNumber (Number Plus "1" Nothing Nothing)
        , JsonArray []
        , JsonObject []
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
      parseTillEnd P.object "{\"a\":null}" `shouldParse` [( "a", JsonNull )]
      parseTillEnd P.object "{ \"a\": null }" `shouldParse` [( "a", JsonNull )]
      parseTillEnd P.object
        "{ \"b\": false \
        \, \"c\": true  \
        \, \"d\": 5     \
        \, \"e\": []    \
        \}              "
        `shouldParse`
        [ ( "b", JsonBoolean False )
        , ( "c", JsonBoolean True )
        , ( "d", JsonNumber (Number Plus "5" Nothing Nothing) )
        , ( "e", JsonArray [] )
        ]

    it "parses nested objects" $ do
      parseTillEnd P.object "{ \"x\": { \"y\": {} } }" `shouldParse` [( "x", JsonObject [( "y", JsonObject [] )] )]

    it "consumes trailing spaces" $ do
      parseTillEnd P.object "{} " `shouldParse` []


valueSpec :: Spec
valueSpec =
  describe "value" $ do
    it "parses a JSON value" $ do
      parseTillEnd P.value "null" `shouldParse` JsonNull
      parseTillEnd P.value "false" `shouldParse` JsonBoolean False
      parseTillEnd P.value "true" `shouldParse` JsonBoolean True
      parseTillEnd P.value "123" `shouldParse` JsonNumber (Number Plus "123" Nothing Nothing)
      parseTillEnd P.value "\"Hello\"" `shouldParse` JsonString "Hello"
      parseTillEnd P.value "[[], null]" `shouldParse` JsonArray [ JsonArray [], JsonNull ]
      parseTillEnd P.value "{ \"a\": null }" `shouldParse` JsonObject [( "a", JsonNull )]

    it "consumes trailing spaces" $ do
      parseTillEnd P.value "null " `shouldParse` JsonNull


errorMessagesSpec :: Spec
errorMessagesSpec =
  --
  -- Test that appropriate helpful error messages are used upon failure
  --
  let
    eJsonLabels :: ET Text
    eJsonLabels =
      elabel "null"
      <> elabel "false"
      <> elabel "true"
      <> elabel "a number"
      <> elabel "a string"
      <> elabel "an array"
      <> elabel "an object"
  in
  describe "error messages" $ do
    it "when failing at the top level" $ do
      parseJson "" `shouldFailWith` err 0 (ueof <> eJsonLabels)

      parseJson "nulL" `shouldFailWith` err 0 (utoks "nulL" <> eJsonLabels)
      parseJson " nulL" `shouldFailWith` err 1 (utoks "nulL" <> eJsonLabels)

      parseJson "faLse" `shouldFailWith` err 0 (utoks "faLse" <> eJsonLabels)
      parseJson "True" `shouldFailWith` err 0 (utoks "True" <> eJsonLabels)
      parseJson "+1." `shouldFailWith` err 0 (utoks "+1." <> eJsonLabels)

    it "when failing in a string" $ do
      parseJson "\"a" `shouldFailWith` err 2
        ( ueof
        <> elabel "a character" <> elabel "a closing quotation mark"
        )

      parseJson "\"a\\\"" `shouldFailWith` err 4
        ( ueof
        <> elabel "a character" <> elabel "a closing quotation mark"
        )

      parseJson "\"a\\x\"" `shouldFailWith` err 3
        ( utok 'x'
        <> elabel "an escape code"
        )

      parseJson "\"a\\u\"" `shouldFailWith` err 4
        ( utok '"'
        <> elabel "a hexadecimal digit"
        )

      parseJson "\"a\\u0\"" `shouldFailWith` err 5
        ( utok '"'
        <> elabel "a hexadecimal digit"
        )

      parseJson "\"a\\u00\"" `shouldFailWith` err 6
        ( utok '"'
        <> elabel "a hexadecimal digit"
        )

      parseJson "\"a\\u002\"" `shouldFailWith` err 7
        ( utok '"'
        <> elabel "a hexadecimal digit"
        )

      parseJson "\"a\\u002G\"" `shouldFailWith` err 7
        ( utok 'G'
        <> elabel "a hexadecimal digit"
        )

    it "when failing in an array" $ do
      parseJson "[" `shouldFailWith` err 1
        ( ueof
        <> etok ']' <> eJsonLabels
        )

      parseJson "[," `shouldFailWith` err 1
        ( utok ','
        <> etok ']' <> eJsonLabels
        )

      parseJson "[,]" `shouldFailWith` err 1
        ( utok ','
        <> etok ']' <> eJsonLabels
        )

      parseJson "[1,]" `shouldFailWith` err 3
        ( utok ']'
        <> eJsonLabels
        )

    it "when failing in an object" $ do
      parseJson "{" `shouldFailWith` err 1
        ( ueof
        <> etok '}' <> elabel "a string"
        )

      parseJson "{123" `shouldFailWith` err 1
        ( utok '1'
        <> etok '}' <> elabel "a string"
        )

      parseJson "{\"a\\x\":}" `shouldFailWith` err 4
        ( utok 'x'
        <> elabel "an escape code"
        )

      parseJson "{\"123\"" `shouldFailWith` err 6
        ( ueof
        <> etok ':'
        )

      parseJson "{\"123\":" `shouldFailWith` err 7
        ( ueof
        <> eJsonLabels
        )

      parseJson "{\"123\":}" `shouldFailWith` err 7
        ( utok '}'
        <> eJsonLabels
        )

      parseJson "{\"123\":123,}" `shouldFailWith` err 11
        ( utok '}'
        <> elabel "a string"
        )


parseTillEnd :: P.Parser a -> Text -> Either P.Error a
parseTillEnd p = parse (p <* eof) ""


--
-- nst/JSONTestSuite
--
-- https://github.com/nst/JSONTestSuite
--


nstJsonTestSuiteSpec :: Spec
nstJsonTestSuiteSpec = do
  maybeDir <- runIO $ lookupEnv "JSON_TEST_SUITE"
  case maybeDir of
    Just dir ->
      buildNstJsonTestSuiteSpec dir

    Nothing ->
      it "skips nst/JSONTestSuite" $ do
        pendingWith "set JSON_TEST_SUITE to run nst/JSONTestSuite"


buildNstJsonTestSuiteSpec :: FilePath -> Spec
buildNstJsonTestSuiteSpec dir = do
  files <- runIO $ sortOn id . filter (".json" `isSuffixOf`) <$> listDirectory dir
  describe "nst/JSONTestSuite" $
    mapM_ (oneTestCase dir) files


oneTestCase :: FilePath -> FilePath -> Spec
oneTestCase dir name = do
  result <- runIO $ getFileContents (dir </> name)
  case name of
    'y' : '_' : _ -> yTestCase name result
    'n' : '_' : _ -> nTestCase name result
    'i' : '_' : _ -> iTestCase name result
    _ -> it name $ expectationFailure $ "unexpected filename: " ++ name


yTestCase :: FilePath -> Either UnicodeException Text -> Spec
yTestCase name (Right content) = it ("parses " ++ name) $ parseJson `shouldSucceedOn` content
yTestCase name (Left e) = it name $ unexpectedUtf8DecodeError e


nTestCase :: FilePath -> Either UnicodeException Text -> Spec
nTestCase name (Right content) = it ("does not parse " ++ name) $ parseJson `shouldFailOn` content
nTestCase name (Left e) =
  let
    containsOneOf :: FilePath -> [String] -> Bool
    containsOneOf s = any (`isInfixOf` s)

    invalidUtf8Substrings :: [String]
    invalidUtf8Substrings =
      [ "invalid-utf8"
      , "invalid_utf8"
      , "invalid-utf-8"
      , "incomplete_UTF8"
      , "lone_continuation_byte"
      , "single_eacute"
      ]
  in
  if name `containsOneOf` invalidUtf8Substrings then
    it ("expects UTF-8 decode error for " ++ name) pass

  else
    it name $ unexpectedUtf8DecodeError e


iTestCase :: FilePath -> Either UnicodeException Text -> Spec
iTestCase name (Right content) =
  if "i_structure_UTF-8_BOM_empty_object" `isPrefixOf` name then
    it ("does not parse " ++ name) $
      parseJson `shouldFailOn` content

  else
    it ("parses " ++ name) $
      parseJson `shouldSucceedOn` content

iTestCase name (Left e) =
  if "i_string" `isPrefixOf` name then
    it ("expects UTF-8 decode error for " ++ name) pass

  else
    it name $ unexpectedUtf8DecodeError e


pass :: Expectation
pass = return ()


unexpectedUtf8DecodeError :: UnicodeException -> Expectation
unexpectedUtf8DecodeError e = expectationFailure ("unexpected UTF-8 decode error " ++ show e)


parseJson :: Text -> Either P.Error Json
parseJson = parse P.json ""


getFileContents :: FilePath -> IO (Either UnicodeException Text)
getFileContents f = TE.decodeUtf8' <$> BS.readFile f
