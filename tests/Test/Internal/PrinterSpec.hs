{-# LANGUAGE OverloadedStrings #-}

module Test.Internal.PrinterSpec (spec) where

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Internal.Printer as P

import Data.Foldable (for_)
import Data.Text.Lazy (Text)
import Internal.Json
import Test.Hspec


spec :: Spec
spec =
  describe "Text.Printer" $ do
    literalsSpec
    numbersSpec
    stringsSpec
    arraysSpec
    objectsSpec


literalsSpec :: Spec
literalsSpec =
  describe "literals" $ do
    itPrettyPrints 0
      [ ( Null, Exactly "null" )
      , ( Boolean False, Exactly "false" )
      , ( Boolean True, Exactly "true" )
      ]


numbersSpec :: Spec
numbersSpec =
  describe "numbers" $
    itPrettyPrints 0
      [ ( Number (Num Plus "1" Nothing Nothing), Exactly "1" )
      , ( Number (Num Minus "1" Nothing Nothing), Exactly "-1" )
      , ( Number (Num Plus "1" (Just "2") Nothing), Exactly "1.2" )
      , ( Number (Num Minus "1" (Just "2") Nothing), Exactly "-1.2" )
      , ( Number (Num Plus "1" Nothing (Just (Plus, "3"))), Exactly "1e3" )
      , ( Number (Num Plus "1" Nothing (Just (Minus, "3"))), Exactly "1e-3" )
      , ( Number (Num Minus "1" Nothing (Just (Plus, "3"))), Exactly "-1e3" )
      , ( Number (Num Minus "1" Nothing (Just (Minus, "3"))), Exactly "-1e-3" )
      , ( Number (Num Plus "1" (Just "2") (Just (Plus, "3"))), Exactly "1.2e3" )
      , ( Number (Num Plus "1" (Just "2") (Just (Minus, "3"))), Exactly "1.2e-3" )
      , ( Number (Num Minus "1" (Just "2") (Just (Plus, "3"))), Exactly "-1.2e3" )
      , ( Number (Num Minus "1" (Just "2") (Just (Minus, "3"))), Exactly "-1.2e-3" )
      ]


stringsSpec :: Spec
stringsSpec =
  describe "strings" $
    itPrettyPrints 0
      [ ( String "a", Exactly "\"a\"" )
      , ( String "/", Exactly "\"/\"" )
      , ( String "\"", Exactly "\"\\\"\"" )
      , ( String "\\", Exactly "\"\\\\\"" )
      , ( String "\b", Exactly "\"\\b\"" )
      , ( String "\f", Exactly "\"\\f\"" )
      , ( String "\n", Exactly "\"\\n\"" )
      , ( String "\r", Exactly "\"\\r\"" )
      , ( String "\t", Exactly "\"\\t\"" )
      , ( String "\x15", Exactly "\"\\u0015\"" )
      , ( String "\x61", Exactly "\"a\"" )
      , ( String "ab\tcd\x65", Exactly "\"ab\\tcde\"" )
      ]


arraysSpec :: Spec
arraysSpec =
  describe "arrays" $ do
    describe "when numSpaces=0" $
      itPrettyPrints 0
        [ ( Array [], Exactly "[]" )
        , ( Array [ Null, Boolean False, Boolean True, Number (Num Plus "1" Nothing Nothing), String "a" ]
          , Exactly "[null,false,true,1,\"a\"]"
          )
        , ( Array [ Array [ Array [] ], Array [] ]
          , Exactly "[[[]],[]]"
          )
        ]

    describe "when numSpaces=4" $
      itPrettyPrints 4
        [ ( Array [], WithDescription "[]" "[]" )
        , ( Array [ Null, Boolean False, Boolean True, Number (Num Plus "1" Nothing Nothing), String "a" ]
          , WithDescription
              "[null,false,true,1,\"a\"]"
              "[\n\
              \    null,\n\
              \    false,\n\
              \    true,\n\
              \    1,\n\
              \    \"a\"\n\
              \]\
              \"
          )
        , ( Array [ Array [ Array [] ], Array [] ]
          , WithDescription
              "[[[]],[]]"
              "[\n\
              \    [\n\
              \        []\n\
              \    ],\n\
              \    []\n\
              \]\
              \"
          )
        ]


objectsSpec :: Spec
objectsSpec =
  describe "objects" $ do
    describe "when numSpaces=0" $
      itPrettyPrints 0
        [ ( Object [], Exactly "{}" )
        , ( Object [ ("a", Null), ("b", Boolean False), ("c", Boolean True), ("d", Number (Num Plus "1" Nothing Nothing)), ("e", String "a") ]
          , Exactly "{\"a\":null,\"b\":false,\"c\":true,\"d\":1,\"e\":\"a\"}"
          )
        , ( Object [ ("a", Object [ ("b", Object [])]) ]
          , Exactly "{\"a\":{\"b\":{}}}"
          )
        ]

    describe "when numSpaces=4" $
      itPrettyPrints 4
        [ ( Object [], WithDescription "{}" "{}" )
        , ( Object [ ("a", Null), ("b", Boolean False), ("c", Boolean True), ("d", Number (Num Plus "1" Nothing Nothing)), ("e", String "a") ]
          , WithDescription
              "{\"a\":null,\"b\":false,\"c\":true,\"d\":1,\"e\":\"a\"}"
              "{\n\
              \    \"a\": null,\n\
              \    \"b\": false,\n\
              \    \"c\": true,\n\
              \    \"d\": 1,\n\
              \    \"e\": \"a\"\n\
              \}\
              \"
          )
        , ( Object [ ("a", Object [ ("b", Object [])]) ]
          , WithDescription
              "{\"a\":{\"b\":{}}}"
              "{\n\
              \    \"a\": {\n\
              \        \"b\": {}\n\
              \    }\n\
              \}\
              \"
          )
        ]



-- Helpers



data Output
  = Exactly Text
  | WithDescription Text Text


itPrettyPrints :: Int -> [(Json, Output)] -> Spec
itPrettyPrints numSpaces cases =
  for_ cases $ \(input, output) ->
    it ("pretty prints " <> toDescription output) $
      TB.toLazyText (P.pretty numSpaces input) `shouldBe` toExpected output


toDescription :: Output -> String
toDescription (Exactly d) = T.unpack d
toDescription (WithDescription d _) = T.unpack d


toExpected :: Output -> Text
toExpected (Exactly e) = e
toExpected (WithDescription _ e) = e
