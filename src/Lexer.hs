module Lexer
  ( Parser, Error
  , SignedNatural(..), Sign(..), signedNatural
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
import Text.Megaparsec ((<?>), Parsec, ParseErrorBundle, satisfy, takeWhileP)
import Text.Megaparsec.Char (char)


type Parser = Parsec Void Text
type Error = ParseErrorBundle Text Void


data SignedNatural = SignedNatural Sign Natural
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
    positiveNumber = toNatural <$> oneNine <*> digits <?> "positive number"

    toNatural :: Char -> Text -> Natural
    toNatural ch = read . T.unpack . T.cons ch

    oneNine :: Parser Char
    oneNine = satisfy isOneNine <?> "non-zero digit"
      where
        isOneNine ch = ch /= '0' && Char.isDigit ch

    digits :: Parser Text
    digits = takeWhileP (Just "digit") Char.isDigit



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
