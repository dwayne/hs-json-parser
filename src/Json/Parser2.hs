module Json.Parser2
    ( Parser, Error
    , ws
    ) where

import Control.Monad (void)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, ParseErrorBundle, takeWhileP)


type Parser = Parsec Void Text


type Error = ParseErrorBundle Text Void


ws :: Parser ()
ws =
  --
  -- ws = *(
  --   %x20 / ; Space
  --   %x09 / ; Horizontal tab
  --   %x0A / ; Line feed or New line
  --   %x0D   ; Carriage return
  -- )
  --
  void $ takeWhileP (Just "white space") isSpace
  where
    isSpace :: Char -> Bool
    isSpace ch =
         ch == '\x20'
      || ch == '\x09'
      || ch == '\x0A'
      || ch == '\x0D'
