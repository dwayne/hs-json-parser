{-# LANGUAGE OverloadedStrings #-}

-- | It provides:
--
-- - A 'Json' data structure for representing JSON.
-- - A JSON parser compliant with <https://www.rfc-editor.org/info/rfc8259/ RFC 8259> which parses JSON into a 'Json' data structure.
-- - A pretty printer for producing well-formatted JSON as 'Text' or within a file.
module Json
  ( -- * JSON
    Json(..)

    -- * Number
  , Number, Sign(..)

    -- ** Construct
  , numFromInt, numFromInteger, numFromFloat, numFromDouble, numFromScientific

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

    -- ** Errors
  , Error(..), SyntaxError

    -- * Printer
  , compact, pretty

    -- ** Write
  , writeCompact, writePretty
  ) where

import qualified Data.ByteString as BS
import qualified Data.Scientific as Scientific
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO.Utf8 as T8
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Read as TR
import qualified Internal.Parser as P
import qualified Internal.Printer as Printer
import qualified Text.Megaparsec as Megaparsec

import Data.Bifunctor (first)
import Data.Char (intToDigit)
import Data.Ratio ((%))
import Data.Scientific (scientific)
import Data.Text (Text)
import Data.Text.Encoding.Error (UnicodeException)
import Data.Text.Lazy (toStrict)
import Data.Void (Void)
import Internal.Json
import Prelude hiding (fromIntegral)
import Text.Megaparsec (ParseErrorBundle)



-- Number: Construct



-- | Construct a t'Number' from an 'Int'.
numFromInt :: Int -> Number
numFromInt n
  | n < 0     = Num Minus (T.show $ -n) Nothing Nothing
  | otherwise = Num Plus (T.show n) Nothing Nothing


-- | Construct a t'Number' from an 'Integer'.
numFromInteger :: Integer -> Number
numFromInteger n
  | n < 0     = Num Minus (T.show $ -n) Nothing Nothing
  | otherwise = Num Plus (T.show n) Nothing Nothing


-- | Construct a t'Number' from a 'Float'.
numFromFloat :: Float -> Number
numFromFloat = numFromRealFloat


-- | Construct a t'Number' from a 'Double'.
numFromDouble :: Double -> Number
numFromDouble = numFromRealFloat


numFromRealFloat :: RealFloat a => a -> Number
numFromRealFloat x =
  let
    s = Scientific.fromFloatDigits x
  in
  numFromScientific (Scientific.coefficient s) (Scientific.base10Exponent s)


-- | Construct a t'Number' from a coefficient, @c@, and a base-10 exponent, @e@.
--
-- The value represented is the 'Fractional' number: @fromInteger c * 10 ^^ e@.
numFromScientific :: Integer -> Int -> Number
numFromScientific coefficient base10Exponent =
  let
    sign :: Sign
    absCoefficient :: Integer
    (sign, absCoefficient) =
      if coefficient < 0 then
        (Minus, -coefficient)

      else
        (Plus, coefficient)

    ds :: [Int]
    e :: Int
    (ds, e) = Scientific.toDecimalDigits $ scientific absCoefficient base10Exponent

    maybeExponent :: Maybe (Sign, Text)
    maybeExponent
      | e == 0    = Nothing
      | e < 0     = Just (Minus, T.show $ -e)
      | otherwise = Just (Plus, T.show e)

    fraction :: Text
    fraction = T.pack $ map intToDigit ds
  in
  Num sign "0" (Just fraction) maybeExponent



-- Number: Convert



-- | Convert a t'Number' to 'Text'.
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


-- | Convert a t'Number' to a 'Rational'.
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



-- Parser



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
parse = Megaparsec.parse P.json ""



-- Parser: Errors



-- | An error that can occur during 'parseFromFile'.
data Error
  = EncodingError UnicodeException -- ^ The input was not valid UTF-8.
  | SyntaxError SyntaxError        -- ^ The input was not valid JSON.
  deriving (Eq, Show)


-- | An error that can occur during 'parse' indicating the input was not valid JSON.
type SyntaxError = ParseErrorBundle Text Void



-- Printer



-- | Render a 'Json' value as 'Text', omitting all whitespace used only for layout.
compact :: Json -> Text
compact =
  pretty 0


-- | Render a 'Json' value as 'Text' with configurable indentation.
--
-- The 'Int' argument is the indentation width. It sets the number of spaces
-- added per nesting level. A width of @0@ produces compact, single-line output;
-- see 'compact'. Negative widths are clamped to @0@.
pretty :: Int -> Json -> Text
pretty numSpaces =
  toStrict . TB.toLazyText . Printer.pretty numSpaces



-- Printer: Write



-- | Write the 'compact' rendering of a 'Json' value to a file, UTF-8 encoded.
writeCompact :: FilePath -> Json -> IO ()
writeCompact f =
  writePretty f 0


-- | Write the 'pretty' rendering of a 'Json' value to a file, UTF-8 encoded.
--
-- The 'Int' argument is the indentation width; see 'pretty'. A positive width
-- appends a trailing newline.
writePretty :: FilePath -> Int -> Json -> IO ()
writePretty f numSpaces json =
  T8.writeFile f $ toStrict $ TB.toLazyText $ Printer.pretty numSpaces json <> newline
  where
    newline :: TB.Builder
    newline =
      if numSpaces > 0 then "\n" else ""
