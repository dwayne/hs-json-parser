module Data.Json (parse) where

import qualified Data.Json.Internal.Parser as P
import qualified Text.Megaparsec as Megaparsec

import Data.Text (Text)


parse :: Text -> Either P.Error P.Json
parse = Megaparsec.parse P.json ""
