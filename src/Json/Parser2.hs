{-# LANGUAGE OverloadedStrings #-}

module Json.Parser2
    ( Parser, Error
    , Json(..), json
    , null, false, true, boolean
    , Number(Number), Sign(..), number
    , string
    , array
    , oneOrMoreWhitespaces
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
import Text.Megaparsec (Parsec, ParseErrorBundle, between, choice, many, notFollowedBy, satisfy, sepBy, takeWhileP, takeWhile1P)
import Text.Megaparsec.Char (alphaNumChar, char)


type Parser = Parsec Void Text


type Error = ParseErrorBundle Text Void


-- Json


data Json
  = JsonNull
  | JsonBoolean Bool
  | JsonNumber Number
  | JsonString Text
  | JsonArray Array
  deriving (Eq, Show)


json :: Parser Json
json =
  choice
    [ JsonNull <$ null
    , JsonBoolean <$> boolean
    , JsonNumber <$> number
    , JsonString <$> string
    , JsonArray <$> array
    ]


-- Literals


null :: Parser Text
null = keyword "null"


false :: Parser Bool
false = False <$ keyword "false"


true :: Parser Bool
true = True <$ keyword "true"


boolean :: Parser Bool
boolean = false <|> true


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
  lexeme (Number <$> leadingSign <*> naturalNumber <*> optional fractionalPart <*> optional exponentPart)
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


-- Strings


string :: Parser Text
string =
  --
  -- string = quotation-mark *char quotation-mark
  --
  -- char = unescaped /
  --   escape (
  --     %x22 /            ; "    quotation mark  U+0022
  --     %x5C /            ; \    reverse solidus U+005C
  --     %x2F /            ; /    solidus         U+002F
  --     %x62 /            ; b    backspace       U+0008
  --     %x66 /            ; f    form feed       U+000C
  --     %x6E /            ; n    line feed       U+000A
  --     %x72 /            ; r    carriage return U+000D
  --     %x74 /            ; t    tab             U+0009
  --     %x75 4HEXDIG )    ; uXXXX                U+XXXX
  --
  -- escape = %x5C         ; \
  --
  -- quotation-mark = %x22 ; "
  --
  -- unescaped = %x20-21 / %x23-5B / %x5D-10FFFF
  --
  lexeme (between quotationMark quotationMark characters)
  where
    characters :: Parser Text
    characters = T.pack <$> many character

    character :: Parser Char
    character = unescaped <|> escaped

    unescaped :: Parser Char
    unescaped = satisfy isUnescaped

    isUnescaped :: Char -> Bool
    isUnescaped ch =
         ch >= '\x20'
      && ch <= '\x10FFFF'
      && ch /= quotationMarkChar
      && ch /= reverseSolidusChar

    escaped :: Parser Char
    escaped = reverseSolidus *> escapeCode

    escapeCode :: Parser Char
    escapeCode = choice
      [ quotationMark
      , reverseSolidus
      , solidus
      , backspace
      , formFeed
      , lineFeed
      , carriageReturn
      , tab
      , unicodeEscape
      ]

    quotationMark :: Parser Char
    quotationMark = char quotationMarkChar

    quotationMarkChar :: Char
    quotationMarkChar = '\x22'

    reverseSolidus :: Parser Char
    reverseSolidus = char reverseSolidusChar

    reverseSolidusChar :: Char
    reverseSolidusChar = '\x5C'

    solidus :: Parser Char
    solidus = char '/'

    backspace :: Parser Char
    backspace = '\x08' <$ char 'b'

    formFeed :: Parser Char
    formFeed = '\x0C' <$ char 'f'

    lineFeed :: Parser Char
    lineFeed = '\x0A' <$ char 'n'

    carriageReturn :: Parser Char
    carriageReturn = '\x0D' <$ char 'r'

    tab :: Parser Char
    tab = '\x09' <$ char 't'

    unicodeEscape :: Parser Char
    unicodeEscape = fromCodePoint <$ char 'u' <*> hexDigit <*> hexDigit <*> hexDigit <*> hexDigit

    fromCodePoint :: Char -> Char -> Char -> Char -> Char
    fromCodePoint a b c d =
      let
        --
        -- N.B. The performance of this could probably be improved by using bit shifts.
        --
        -- The key insight is to notice that 16^3 = (2^4)^3 = 2^(4*3) = 2^12.
        --
        -- Hence,
        --
        --   Char.digitToInt a * 4096 == Char.digitToInt a `shiftL` 12
        --
        -- , where shiftL comes from Data.Bits.
        --
        n =
            Char.digitToInt a * 4096 -- (4096 = 16^3)
          + Char.digitToInt b * 256  -- ( 256 = 16^2)
          + Char.digitToInt c * 16   -- (  16 = 16^1)
          + Char.digitToInt d        -- (   1 = 16^0)
      in
      Char.chr n

    hexDigit :: Parser Char
    hexDigit = satisfy Char.isHexDigit


-- Structural characters


beginArray :: Parser Text
beginArray =
  symbol "[" -- left square bracket


endArray :: Parser Text
endArray =
  symbol "]" -- right square bracket


valueSeparator :: Parser Text
valueSeparator =
  symbol "," -- comma


beginObject :: Parser Text
beginObject =
  symbol "{" -- left curly bracket


endObject :: Parser Text
endObject =
  symbol "}" -- right curly bracket


nameSeparator :: Parser Text
nameSeparator =
  symbol ":" -- colon


-- Arrays


type Array = [Json]


array :: Parser Array
array =
  --
  -- array = begin-array [ value *( value-separator value ) ] end-array
  --
  between beginArray endArray (json `sepBy` valueSeparator)


-- Lexeme parsers


keyword :: Text -> Parser Text
keyword kw = lexeme (Char.string kw <* notFollowedBy alphaNumChar)


symbol :: Text -> Parser Text
symbol = L.symbol sc


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc


sc :: Parser ()
sc = L.space oneOrMoreWhitespaces empty empty


oneOrMoreWhitespaces :: Parser ()
oneOrMoreWhitespaces =
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
