module Internal.Data.Json
  ( Json(..)
  , Number(..), Sign(..)
  , Array
  , Object
  ) where

import Data.Text (Text)


data Json
  = JsonNull
  | JsonBoolean Bool
  | JsonNumber Number
  | JsonString Text
  | JsonArray Array
  | JsonObject Object
  deriving (Eq, Show)


data Number
  = Number
      { numSign :: Sign
      , numDigits :: Text
      , numFraction :: Maybe Text
      , numExponent :: Maybe (Sign, Text)
      }
  deriving (Eq, Show)


data Sign
  = Plus
  | Minus
  deriving (Eq, Show)


type Array = [Json]


type Object = [(Text, Json)]
