module Lexer (ws) where

import Control.Monad (void)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, takeWhileP)


type Parser = Parsec Void Text


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
