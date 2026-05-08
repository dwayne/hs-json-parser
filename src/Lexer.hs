module Lexer
  ( Parser, Error
  , SignedNatural(..), Sign(..), signedNatural
  , FractionalPart(..), fraction
  , sign
  , ws
  ) where

import qualified Data.Char as Char
import qualified Data.Text as T

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Text (Text)
import Data.Void (Void)
import Numeric.Natural (Natural)
import Text.Megaparsec ((<?>), Parsec, ParseErrorBundle, satisfy, takeWhileP, takeWhile1P)
import Text.Megaparsec.Char (char)


type Parser = Parsec Void Text
type Error = ParseErrorBundle Text Void


data SignedNatural = SignedNatural Sign Natural
  deriving (Eq, Show)


--
-- FractionalPart n k == n * 10 ^^ (-k)
--
-- For e.g.
--
-- FractionalPart 5 1 == 5.0 * 10 ^^ (-1)
--                    == 0.5
--
data FractionalPart = FractionalPart Natural Int
  deriving (Eq, Show)


data Sign
  = Plus
  | Minus
  deriving (Eq, Show)


signedNatural :: Parser SignedNatural
signedNatural =
  --
  -- integer
  --     digit
  --     onenine digits
  --     '-' digit
  --     '-' onenine digits
  --
  -- digits
  --     digit
  --     digit digits
  --
  -- digit
  --     '0'
  --     onenine
  --
  -- onenine
  --     '1' . '9'
  --
  SignedNatural <$> sign <*> (zero <|> positiveNumber)
  where
    sign :: Parser Sign
    sign = Minus <$ char '-' <|> pure Plus <?> "sign"

    zero :: Parser Natural
    zero = 0 <$ char '0' <?> "zero"

    positiveNumber :: Parser Natural
    positiveNumber = toNatural <$> oneNine <*> digits0 <?> "positive number"

    toNatural :: Char -> Text -> Natural
    toNatural ch = read . T.unpack . T.cons ch

    oneNine :: Parser Char
    oneNine = satisfy isOneNine <?> "non-zero digit"
      where
        isOneNine ch = ch /= '0' && Char.isDigit ch

    --
    -- zero or more digits
    --
    digits0 :: Parser Text
    digits0 = takeWhileP (Just "digit") Char.isDigit


fraction :: Parser (Maybe FractionalPart)
fraction =
  --
  -- fraction
  --   ""
  --   '.' digits
  --
  optional fractionalPart


fractionalPart :: Parser FractionalPart
fractionalPart =
  --
  -- '.' digits
  --
  toFractionalPart <$> (char '.' *> digits1) <?> "fractional part"
  where
    toFractionalPart :: Text -> FractionalPart
    toFractionalPart t =
      let
        n = read (T.unpack t)
        k = T.length t
      in
      if n == 0 then
        FractionalPart 0 0

      else
        FractionalPart n k


--
-- one or more digits
--
digits1 :: Parser Text
digits1 = takeWhile1P (Just "digit") Char.isDigit


sign :: Parser (Maybe Sign)
sign =
  --
  -- sign
  --     ""
  --     '+'
  --     '-'
  --
  plus <|> minus <|> pure Nothing
  where
    plus :: Parser (Maybe Sign)
    plus = Just Plus <$ char '+' <?> "plus sign"

    minus :: Parser (Maybe Sign)
    minus = Just Minus <$ char '-' <?> "minus sign"


ws :: Parser ()
ws =
  --
  -- ws
  --     ""
  --     '0020' ws
  --     '000A' ws
  --     '000D' ws
  --     '0009' ws
  --
  void $ takeWhileP (Just "white space") isSpace
  where
    isSpace :: Char -> Bool
    isSpace ch =
         ch == '\x0020'
      || ch == '\x000A'
      || ch == '\x000D'
      || ch == '\x0009'


-- Helpers


optional :: Parser a -> Parser (Maybe a)
optional p = (Just <$> p) <|> pure Nothing
