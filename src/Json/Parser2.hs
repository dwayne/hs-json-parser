{-# LANGUAGE OverloadedStrings #-}

module Json.Parser2
    ( Parser, Error
    , false, true, null
    , Number(Number), Sign(..), number
    , ws1
    ) where

import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Applicative ((<|>), empty)
import Control.Monad (void)
import Data.Text (Text)
import Data.Void (Void)
import Prelude hiding (null)
import Text.Megaparsec (Parsec, ParseErrorBundle, notFollowedBy, satisfy, takeWhileP, takeWhile1P)
import Text.Megaparsec.Char (alphaNumChar, char)


type Parser = Parsec Void Text


type Error = ParseErrorBundle Text Void


-- Numbers


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


number :: Parser Number
number =
  --
  -- number = [ minus ] int [ frac ] [ exp ]
  --
  -- minus = %x2D         ; -
  --
  -- int      = zero / ( digit1-9 *DIGIT )
  -- zero     = %x30      ; 0
  -- digit1-9 = %x31-39   ; 1-9
  --
  -- frac          = decimal-point 1*DIGIT
  -- decimal-point = %x2E ; .
  --
  -- exp  = e [ minus / plus ] 1*DIGIT
  -- e    = %x65 / %x45   ; e E
  -- plus = %x2B          ; +
  --
  Number <$> leadingSign <*> naturalNumber <*> optional fractionalPart <*> optional exponentPart
  where
    leadingSign :: Parser Sign
    leadingSign = minus <|> pure Plus

    naturalNumber :: Parser Text
    naturalNumber = zero <|> positiveNumber

    zero :: Parser Text
    zero = Char.string "0"

    positiveNumber :: Parser Text
    positiveNumber = T.cons <$> oneToNine <*> zeroOrMoreDigits

    oneToNine :: Parser Char
    oneToNine = satisfy isNonZeroDigit

    isNonZeroDigit :: Char -> Bool
    isNonZeroDigit ch = Char.isDigit ch && ch /= '0'

    zeroOrMoreDigits :: Parser Text
    zeroOrMoreDigits = takeWhileP (Just "digit") Char.isDigit

    fractionalPart :: Parser Text
    fractionalPart = char '.' *> oneOrMoreDigits

    exponentPart :: Parser (Sign, Text)
    exponentPart = (,) <$ e <*> exponentSign <*> oneOrMoreDigits

    e :: Parser Char
    e = char 'E' <|> char 'e'

    exponentSign :: Parser Sign
    exponentSign = minus <|> plus <|> pure Plus

    minus :: Parser Sign
    minus = Minus <$ char '-'

    plus :: Parser Sign
    plus = Plus <$ char '+'

    oneOrMoreDigits :: Parser Text
    oneOrMoreDigits = takeWhile1P (Just "digit") Char.isDigit


-- Literals


false :: Parser Bool
false = False <$ keyword "false"


true :: Parser Bool
true = True <$ keyword "true"


null :: Parser ()
null = void $ keyword "null"


-- Structural characters


beginArray :: Parser Text
beginArray =
  symbol "[" -- left square bracket


endArray :: Parser Text
endArray =
  symbol "]" -- right square bracket


beginObject :: Parser Text
beginObject =
  symbol "{" -- left curly bracket


endObject :: Parser Text
endObject =
  symbol "}" -- right curly bracket


nameSeparator :: Parser Text
nameSeparator =
  symbol ":" -- colon


valueSeparator :: Parser Text
valueSeparator =
  symbol "," -- comma


-- Lexeme parsers


keyword :: Text -> Parser Text
keyword kw = lexeme (Char.string kw <* notFollowedBy alphaNumChar)


symbol :: Text -> Parser Text
symbol = L.symbol sc


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc


sc :: Parser ()
sc = L.space ws1 empty empty


ws1 :: Parser ()
ws1 =
  --
  -- ws = 1*(
  --   %x20 / ; Space
  --   %x09 / ; Horizontal tab
  --   %x0A / ; Line feed or New line
  --   %x0D   ; Carriage return
  -- )
  --
  void $ takeWhile1P (Just "white space") isSpace
  where
    isSpace :: Char -> Bool
    isSpace ch =
         ch == '\x20'
      || ch == '\x09'
      || ch == '\x0A'
      || ch == '\x0D'


-- Helpers


optional :: Parser a -> Parser (Maybe a)
optional p = (Just <$> p) <|> pure Nothing
