{-# LANGUAGE OverloadedStrings #-}

-- | A JSON parser compliant with <https://www.rfc-editor.org/info/rfc8259/ RFC 8259>.
module Data.Json
  ( -- * JSON
    Json(..)

    -- * Number
  , Number, Sign(..)

    -- ** Construct
  , numFromInt, numFromInteger

    -- ** Query
  , numSign, numDigits, numFraction, numExponent

    -- ** Convert
  , numToText, numToRational

    -- * Array
  , Array

    -- * Object
  , Object

    -- * Parser
  , parse, parseFromFile

    -- * Errors
  , Error(..), SyntaxError
  ) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import qualified Internal.Data.Json as Json
import qualified Internal.Text.Parser as P
import qualified Text.Megaparsec as Megaparsec

import Data.Bifunctor (first, second)
import Data.Ratio ((%))
import Data.Text (Text)
import Data.Text.Encoding.Error (UnicodeException)
import Data.Void (Void)
import Prelude hiding (fromIntegral)
import Text.Megaparsec (ParseErrorBundle)


-- | A JSON <https://www.rfc-editor.org/info/rfc8259/#section-3 value>.
data Json
  = Null
  | Boolean Bool
  | Number Number
  | String Text
  | Array Array
  | Object Object
  deriving (Eq, Show)


-- | A JSON <https://www.rfc-editor.org/info/rfc8259/#section-6 number>.
data Number
  = Num
      { numSign :: Sign                   -- ^ The sign.
      , numDigits :: Text                 -- ^ The integer part.
      , numFraction :: Maybe Text         -- ^ The fractional part, if any.
      , numExponent :: Maybe (Sign, Text) -- ^ The exponent, if any.
      }
  deriving (Eq, Show)


-- | The sign of a 'Number' or its exponent: positive ('Plus') or negative ('Minus').
data Sign
  = Plus
  | Minus
  deriving (Eq, Show)


-- | Construct a 'Number' from an 'Int'.
numFromInt :: Int -> Number
numFromInt n
  | n < 0     = Num Minus (T.show $ -n) Nothing Nothing
  | otherwise = Num Plus (T.show n) Nothing Nothing


-- | Construct a 'Number' from an 'Integer'.
numFromInteger :: Integer -> Number
numFromInteger n
  | n < 0     = Num Minus (T.show $ -n) Nothing Nothing
  | otherwise = Num Plus (T.show n) Nothing Nothing


-- | Convert a 'Number' to 'Text'.
numToText :: Number -> Text
numToText (Num s d mf me) =
  signToText s <> d <> fractionToText mf <> exponentToText me
  where
    signToText :: Sign -> Text
    signToText Plus  = ""
    signToText Minus = "-"

    fractionToText :: Maybe Text -> Text
    fractionToText Nothing  = ""
    fractionToText (Just f) = "." <> f

    exponentToText :: Maybe (Sign, Text) -> Text
    exponentToText Nothing           = ""
    exponentToText (Just (Plus, e))  = "e" <> e
    exponentToText (Just (Minus, e)) = "e-" <> e


-- | Convert a 'Number' to a 'Rational'.
numToRational :: Number -> Rational
numToRational (Num s d mf me) =
  if n < 0 then
    numer % pow10 (-n)

  else
    (numer * pow10 n) % 1
  where
    numer  = signToInteger s * textToInteger (d <> f)
    n      = e - l
    (f, l) = fractionToTextWithLength mf
    e      = exponentToInteger me

    signToInteger :: Sign -> Integer
    signToInteger Plus  = 1
    signToInteger Minus = -1

    fractionToTextWithLength :: Maybe Text -> (Text, Integer)
    fractionToTextWithLength Nothing        = ("", 0)
    fractionToTextWithLength (Just fDigits) = (fDigits, toInteger $ T.length fDigits)

    exponentToInteger :: Maybe (Sign, Text) -> Integer
    exponentToInteger Nothing                 = 0
    exponentToInteger (Just (eSign, eDigits)) = signToInteger eSign * textToInteger eDigits

    pow10 :: Integer -> Integer
    pow10 p = 10 ^ p

    textToInteger :: Text -> Integer
    textToInteger = either (const 0) fst . TR.decimal


-- | A JSON <https://www.rfc-editor.org/info/rfc8259/#section-5 array>.
type Array = [Json]


-- | A JSON <https://www.rfc-editor.org/info/rfc8259/#section-4 object>.
--
-- Represented as a list of name/value pairs, preserving insertion order
-- and allowing duplicate keys, as permitted by the RFC.
type Object = [(Text, Json)]


-- | An error that can occur during 'parseFromFile'.
data Error
  = EncodingError UnicodeException -- ^ The input was not valid UTF-8.
  | SyntaxError SyntaxError        -- ^ The input was not valid JSON.
  deriving (Eq, Show)


-- | An error that can occur during 'parse' indicating the input was not valid JSON.
type SyntaxError = ParseErrorBundle Text Void


-- | Parse a JSON value from a file.
parseFromFile :: FilePath -> IO (Either Error Json)
parseFromFile f = do
  bs <- BS.readFile f
  case TE.decodeUtf8' bs of
    Right content ->
      return $ first SyntaxError (parse content)

    Left err ->
      return $ Left (EncodingError err)


-- | Parse a JSON value from 'Text'.
parse :: Text -> Either SyntaxError Json
parse = fmap convertJson . Megaparsec.parse P.json ""


convertJson :: Json.Json -> Json
convertJson Json.Null        = Null
convertJson (Json.Boolean b) = Boolean b
convertJson (Json.Number n)  = Number $ convertNumber n
convertJson (Json.String t)  = String t
convertJson (Json.Array a)   = Array $ map convertJson a
convertJson (Json.Object o)  = Object $ map (second convertJson) o


convertNumber :: Json.Number -> Number
convertNumber (Json.Num s d f e) =
  Num (convertSign s) d f (fmap (first convertSign) e)


convertSign :: Json.Sign -> Sign
convertSign Json.Plus  = Plus
convertSign Json.Minus = Minus
