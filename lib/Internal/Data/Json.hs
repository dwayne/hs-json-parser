module Internal.Data.Json
  ( Json(..)
  , Number(..), Sign(..)
  , Array
  , Object
  ) where

import Data.Text (Text)


data Json
  = Null
  | Boolean Bool
  | Number Number
  | String Text
  | Array Array
  | Object Object
  deriving (Eq, Show)


data Number
  = Num
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
