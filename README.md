# JSON Parser

A JSON parser compliant with [RFC 8259](https://www.rfc-editor.org/info/rfc8259/).

## Usage

This package is distributed through its Git repository rather than Hackage. Add a `source-repository-package` stanza to your `cabal.project` to use it.

In `cabal.project`:

```
source-repository-package
  type: git
  location: git@github.com:dwayne/hs-json-parser.git
  [tag: <hash|tag|branch>]
```

**N.B.** _Omitting the tag tracks the default branch and sacrifices reproducibility._

In `example.cabal`:

```
build-depends:
  json-parser
```

## Parsing

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Json as Json

Json.parse "null"
-- Right Null

Json.parse "false"
-- Right (Boolean False)

Json.parse "true"
-- Right (Boolean True)

Json.parse "1.2345e-67890"
-- Right (Number (Num { numSign = Plus, numDigits = "1", numFraction = Just "2345", numExponent = Just (Minus,"67890") }))

Json.parse "\"Hask\\u0065ll\""
-- Right (String "Haskell")

Json.parse "[true, false, true]"
-- Right (Array [ Boolean True, Boolean False, Boolean True ])

Json.parse "{\"name\": \"Douglas\", \"age\": 71, \"email\": null}"
-- Right
--   (Object
--     [ ( "name", String "Douglas" )
--     , ( "age", Number (Num { numSign = Plus, numDigits = "71", numFraction = Nothing, numExponent = Nothing }) )
--     , ( "email", Null )
--     ]
--   )
```

## Printing

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Json as Json
import qualified Data.Text.IO as TIO

import Data.Json (Json(..))

TIO.putStrLn $ Json.compact
  (Object
    [ ( "name", String "Douglas" )
    , ( "age", Number (Json.numFromInt 71) )
    , ( "email", Null )
    ]
  )
-- {"name":"Douglas","age":71,"email":null}

TIO.putStrLn $ Json.pretty 4
  (Object
    [ ( "name", String "Douglas" )
    , ( "age", Number (Json.numFromInt 71) )
    , ( "email", Null )
    ]
  )
-- {
--     "name": "Douglas",
--     "age": 71,
--     "email": null
-- }
```

## Public API

```haskell
data Json
  = Null
  | Boolean Bool
  | Number Number
  | String Text
  | Array Array
  | Object Object
  deriving (Eq, Show)

data Number

instance Eq Number
instance Show Number

data Sign
  = Plus
  | Minus
  deriving (Eq, Show)

type Array = [Json]

type Object = [(Text, Json)]

data Error
  = EncodingError UnicodeException
  | SyntaxError SyntaxError
  deriving (Eq, Show)

type SyntaxError = ParseErrorBundle Text Void

-- Number: Construct

numFromInt :: Int -> Number
numFromInteger :: Integer -> Number
numFromFloat :: Float -> Number
numFromDouble :: Double -> Number
numFromScientific :: Integer -> Int -> Number

-- Number: Query

numSign :: Number -> Sign
numDigits :: Number -> Text
numFraction :: Number -> Maybe Text
numExponent :: Number -> Maybe (Sign, Text)

-- Number: Convert

numToText :: Number -> Text
numToRational :: Number -> Rational

-- Parser

parse :: Text -> Either SyntaxError Json
parseFromFile :: FilePath -> IO (Either Error Json)

-- Printer

compact :: Json -> Text
pretty :: Int -> Json -> Text

-- Printer: Write

writeCompact :: FilePath -> Json -> IO ()
writePretty :: FilePath -> Int -> Json -> IO ()
```

- [`UnicodeException`](https://hackage-content.haskell.org/package/text-2.1.4/docs/Data-Text-Encoding-Error.html#t:UnicodeException)
