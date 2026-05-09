{-# LANGUAGE OverloadedStrings #-}

module Lexer
  ( Parser, Error
  , Number(..), number
  , SignedNatural(..), Sign(..), signedNatural
  , FractionalPart(..), fractionalPart
  , ExponentPart(..), exponentPart
  , boolean
  , ws
  ) where

import Prelude hiding (exponent)

import qualified Data.Char as Char
import qualified Data.Text as T

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Text (Text)
import Data.Void (Void)
import Numeric.Natural (Natural)
import Text.Megaparsec
  ( (<?>)
  , Parsec, ParseErrorBundle
  , notFollowedBy
  , satisfy, takeWhileP, takeWhile1P
  )
import Text.Megaparsec.Char (alphaNumChar, char, string)


type Parser = Parsec Void Text
type Error = ParseErrorBundle Text Void


data Number = Number SignedNatural (Maybe FractionalPart) (Maybe ExponentPart)
  deriving (Eq, Show)


data SignedNatural = SignedNatural Sign Text
  deriving (Eq, Show)


--
-- FractionalPart n represents [n] * 10.0 ^^ (- length n)
--
-- where [n] is the natural number corresponding to n
--
-- For e.g.
--
-- FractionalPart "5" ["5"] * 10.0 ^^ (-1) = 5 * 10.0 ^^ (-1) = 0.5
--
data FractionalPart = FractionalPart Text
  deriving (Eq, Show)


--
-- ExponentPart Plus "3" represents 10.0 ^^ 3
-- ExponentPart Minus "3" represents 10.0 ^^ (-3)
--
data ExponentPart = ExponentPart Sign Text
  deriving (Eq, Show)


data Sign
  = Plus
  | Minus
  deriving (Eq, Show)


boolean :: Parser Bool
boolean =
  True <$ keyword "true" <|> False <$ keyword "false" <?> "boolean"


number :: Parser Number
number =
  Number <$> signedNatural <*> fraction <*> exponent <?> "number"


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

    zero :: Parser Text
    zero = "0" <$ char '0' <?> "zero"

    positiveNumber :: Parser Text
    positiveNumber = T.cons <$> oneNine <*> digits0 <?> "positive number"

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
  FractionalPart <$> (char '.' *> digits1) <?> "fractional part"


exponent :: Parser (Maybe ExponentPart)
exponent =
  --
  -- exponent
  --   ""
  --   'E' sign digits
  --   'e' sign digits
  --
  optional exponentPart


exponentPart :: Parser ExponentPart
exponentPart =
  --
  -- 'E' sign digits
  -- 'e' sign digits
  --
  ExponentPart <$> (e *> sign) <*> digits1 <?> "exponent part"
  where
    e :: Parser Char
    e = char 'E' <|> char 'e'

    sign :: Parser Sign
    sign = (Plus <$ char '+') <|> (Minus <$ char '-') <|> pure Plus <?> "sign"


--
-- one or more digits
--
digits1 :: Parser Text
digits1 = takeWhile1P (Just "digit") Char.isDigit


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


keyword :: Text -> Parser Text
keyword kw = string kw <* notFollowedBy alphaNumChar <?> T.unpack kw
