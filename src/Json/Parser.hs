{-# LANGUAGE OverloadedStrings #-}

module Json.Parser
  ( Parser, Error
  , Json(..), json
  , Array, array
  , Number(..), number
  , SignedNatural(..), Sign(..), signedNatural
  , FractionalPart(..), fractionalPart
  , ExponentPart(..), exponentPart
  , boolean
  , null
  , ws
  ) where

import Prelude hiding (exponent, null)

import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Applicative ((<|>), empty)
import Control.Monad (void)
import Data.Text (Text)
import Data.Void (Void)
import Numeric.Natural (Natural)
import Text.Megaparsec
  ( (<?>)
  , Parsec, ParseErrorBundle
  , between, choice, sepBy
  , notFollowedBy
  , satisfy, takeWhileP, takeWhile1P
  )
import Text.Megaparsec.Char (alphaNumChar, char, string)


type Parser = Parsec Void Text
type Error = ParseErrorBundle Text Void


data Json
  = JsonArray Array
  | JsonNumber Number
  | JsonBoolean Bool
  | JsonNull
  deriving (Eq, Show)


type Array = [Json]


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


json :: Parser Json
json =
  --
  -- json
  --     ws element
  --
  ws *> element


element :: Parser Json
element =
  --
  -- element
  --     value ws
  --
  lexeme value


value :: Parser Json
value =
  --
  -- value
  --     object
  --     array
  --     string
  --     number
  --     "true"
  --     "false"
  --     "null"
  --
  choice
    [ JsonArray <$> array
    , JsonNumber <$> number
    , JsonBoolean <$> boolean
    , JsonNull <$ null
    ]


array :: Parser Array
array =
  --
  -- array
  --     '[' ws ']' ws
  --     '[' ws elements ']' ws
  --
  between (symbol "[") (symbol "]") elements


elements :: Parser [Json]
elements =
  --
  -- elements
  --     element
  --     element ',' ws elements
  --
  element `sepBy` (symbol ",")


number :: Parser Number
number =
  --
  -- number
  --     integer fraction exponent
  --
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
  --     ""
  --     '.' digits
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
  --     ""
  --     'E' sign digits
  --     'e' sign digits
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


digits1 :: Parser Text
digits1 =
  --
  -- one or more digits
  --
  takeWhile1P (Just "digit") Char.isDigit


boolean :: Parser Bool
boolean =
  --
  -- "true"
  -- "false"
  --
  True <$ keyword "true" <|> False <$ keyword "false" <?> "boolean"


null :: Parser ()
null =
  --
  -- "null"
  --
  void (keyword "null") <?> "null"


keyword :: Text -> Parser Text
keyword kw = string kw <* notFollowedBy alphaNumChar <?> T.unpack kw


-- Lexeme


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc


symbol :: Text -> Parser Text
symbol = L.symbol sc


sc :: Parser ()
sc = L.space ws1 empty empty


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


ws1 :: Parser ()
ws1 =
  --
  -- ws
  --     '0020' ws
  --     '000A' ws
  --     '000D' ws
  --     '0009' ws
  --
  void $ takeWhile1P (Just "white space") isSpace


isSpace :: Char -> Bool
isSpace ch =
     ch == '\x0020'
  || ch == '\x000A'
  || ch == '\x000D'
  || ch == '\x0009'


-- Helpers


optional :: Parser a -> Parser (Maybe a)
optional p = (Just <$> p) <|> pure Nothing
