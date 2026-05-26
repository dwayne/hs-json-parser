{-# LANGUAGE OverloadedStrings #-}

{- | TODO
-}
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
  , Error, SyntaxError
  ) where

import qualified Data.ByteString as BS
import qualified Data.Json.Internal.Parser as P
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import qualified Text.Megaparsec as Megaparsec

import Data.Bifunctor (first, second)
import Data.Ratio ((%))
import Data.Text (Text)
import Data.Text.Encoding.Error (UnicodeException)
import Data.Void (Void)
import Prelude hiding (fromIntegral)
import Text.Megaparsec (ParseErrorBundle)


-- | A JSON value.
data Json
  = JsonNull
  | JsonBoolean Bool
  | JsonNumber Number
  | JsonString Text
  | JsonArray Array
  | JsonObject Object
  deriving (Eq, Show)


-- | A JSON number.
data Number
  = Number
      { numSign :: Sign -- ^ Get the sign of the number.
      , numDigits :: Text -- ^ Get the digits before the decimal point.
      , numFraction :: Maybe Text -- ^ Get the digits after the decimal point, if any.
      , numExponent :: Maybe (Sign, Text) -- ^ Get the exponent, if any.
      }
  deriving (Eq, Show)


-- | TODO
numFromInt :: Int -> Number
numFromInt n
  | n < 0     = Number Minus (T.show $ -n) Nothing Nothing
  | otherwise = Number Plus (T.show n) Nothing Nothing


-- | TODO
numFromInteger :: Integer -> Number
numFromInteger n
  | n < 0     = Number Minus (T.show $ -n) Nothing Nothing
  | otherwise = Number Plus (T.show n) Nothing Nothing


-- | TODO
numToText :: Number -> Text
numToText (Number s d mf me) =
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


-- | TODO
numToRational :: Number -> Rational
numToRational (Number s d mf me) =
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


-- | A sign.
data Sign
  = Plus
  | Minus
  deriving (Eq, Show)


-- | TODO
type Array = [Json]


-- | TODO
type Object = [(Text, Json)]


-- | TODO
data Error
  = EncodingError UnicodeException
  | SyntaxError SyntaxError
  deriving (Eq, Show)


-- | TODO
type SyntaxError = ParseErrorBundle Text Void


-- | TODO
parseFromFile :: FilePath -> IO (Either Error Json)
parseFromFile f = do
  bs <- BS.readFile f
  case TE.decodeUtf8' bs of
    Right content ->
      return $ first SyntaxError (parse content)

    Left err ->
      return $ Left (EncodingError err)


-- | TODO
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
