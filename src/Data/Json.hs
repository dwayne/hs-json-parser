module Data.Json
  ( Json(..)
  , Number(..), Sign(..)
  , Array
  , Object
  , SyntaxError
  , parse
  ) where

import qualified Data.Json.Internal.Parser as P
import qualified Text.Megaparsec as Megaparsec

import Data.Bifunctor (first, second)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle)


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


type SyntaxError = ParseErrorBundle Text Void


parse :: Text -> Either SyntaxError Json
parse = fmap convertJson . Megaparsec.parse P.json ""


convertJson :: P.Json -> Json
convertJson P.JsonNull        = JsonNull
convertJson (P.JsonBoolean b) = JsonBoolean b
convertJson (P.JsonNumber n)  = JsonNumber $ convertNumber n
convertJson (P.JsonString t)  = JsonString t
convertJson (P.JsonArray a)   = JsonArray $ map convertJson a
convertJson (P.JsonObject o)  = JsonObject $ map (second convertJson) o


convertNumber :: P.Number -> Number
convertNumber (P.Number s d f e) =
  Number (convertSign s) d f (fmap (first convertSign) e)


convertSign :: P.Sign -> Sign
convertSign P.Plus  = Plus
convertSign P.Minus = Minus
