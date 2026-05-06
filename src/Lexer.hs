module Lexer (Sign(..), sign, ws) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec ((<?>), Parsec, takeWhileP)
import Text.Megaparsec.Char (char)


type Parser = Parsec Void Text


data Sign
  = Plus
  | Minus
  deriving (Eq, Show)


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
