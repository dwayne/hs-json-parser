module Lexer where

import qualified Data.Char as Char
import qualified Text.Parsec as P
import qualified Text.Parsec.Language as L
import qualified Text.Parsec.Token as T

import Text.Parsec ((<?>))
import Text.Parsec.String (Parser)


-- Token Parser


tokenParser :: T.TokenParser ()
tokenParser = T.makeTokenParser jsonDef


jsonDef :: L.LanguageDef ()
jsonDef =
  L.emptyDef
    { T.identStart = letter
    , T.identLetter = letterOrDigit
    , T.reservedNames =
        [ "false"
        , "null"
        , "true"
        ]
    }


letter :: Parser Char
letter = P.satisfy Char.isAlpha <?> "letter"


letterOrDigit :: Parser Char
letterOrDigit = P.satisfy Char.isAlphaNum <?> "letter or digit"
