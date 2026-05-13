module Json.Parser2
    ( Parser, Error
    , ws1
    ) where

import qualified Text.Megaparsec.Char.Lexer as L

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, ParseErrorBundle, takeWhile1P)


type Parser = Parsec Void Text


type Error = ParseErrorBundle Text Void


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
