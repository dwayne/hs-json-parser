{-# LANGUAGE OverloadedStrings #-}

module Sandbox where

import Control.Applicative ((<|>))
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec)
import Text.Megaparsec.Char (string)


type Parser = Parsec Void Text


scheme :: Parser Text
scheme =
  string "data"
  <|> string "file"
  <|> string "ftp"
  <|> string "http"
  <|> string "https"
  <|> string "irc"
  <|> string "mailto"
