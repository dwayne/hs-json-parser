{-# LANGUAGE OverloadedStrings #-}

module Test.Internal.Text.PrinterSpec (spec) where

import qualified Data.Text.Lazy as T
import qualified Internal.Text.Printer as P

import Data.Foldable (for_)
import Data.Text.Lazy (Text)
import Internal.Data.Json
import Test.Hspec


spec :: Spec
spec =
  describe "Text.Printer" $ do
    literalsSpec
    numbersSpec
    stringsSpec


literalsSpec :: Spec
literalsSpec =
  describe "literals" $ do
    itPrettyPrints 0
      [ ( Null, "null" )
      , ( Boolean False, "false" )
      , ( Boolean True, "true" )
      ]


numbersSpec :: Spec
numbersSpec =
  describe "numbers" $
    itPrettyPrints 0
      [ ( Number (Num Plus "1" Nothing Nothing), "1" )
      , ( Number (Num Minus "1" Nothing Nothing), "-1" )
      , ( Number (Num Plus "1" (Just "2") Nothing), "1.2" )
      , ( Number (Num Minus "1" (Just "2") Nothing), "-1.2" )
      , ( Number (Num Plus "1" Nothing (Just (Plus, "3"))), "1e3" )
      , ( Number (Num Plus "1" Nothing (Just (Minus, "3"))), "1e-3" )
      , ( Number (Num Minus "1" Nothing (Just (Plus, "3"))), "-1e3" )
      , ( Number (Num Minus "1" Nothing (Just (Minus, "3"))), "-1e-3" )
      , ( Number (Num Plus "1" (Just "2") (Just (Plus, "3"))), "1.2e3" )
      , ( Number (Num Plus "1" (Just "2") (Just (Minus, "3"))), "1.2e-3" )
      , ( Number (Num Minus "1" (Just "2") (Just (Plus, "3"))), "-1.2e3" )
      , ( Number (Num Minus "1" (Just "2") (Just (Minus, "3"))), "-1.2e-3" )
      ]


stringsSpec :: Spec
stringsSpec =
  describe "strings" $
    itPrettyPrints 0
      [ ( String "a", "\"a\"" )
      , ( String "\"", "\"\\\"\"" )
      , ( String "\\", "\"\\\\\"" )
      , ( String "/", "\"\\/\"" )
      , ( String "\b", "\"\\b\"" )
      , ( String "\f", "\"\\f\"" )
      , ( String "\n", "\"\\n\"" )
      , ( String "\r", "\"\\r\"" )
      , ( String "\t", "\"\\t\"" )
      , ( String "\x15", "\"\\u0015\"" )
      , ( String "\x61", "\"a\"" )
      , ( String "ab\tcd\x65", "\"ab\\tcde\"" )
      ]



-- Helpers



itPrettyPrints :: Int -> [(Json, Text)] -> Spec
itPrettyPrints numSpaces cases =
  for_ cases $ \(input, expected) ->
    it ("pretty prints " <> T.unpack expected) $
      P.pretty numSpaces input `shouldBe` expected
