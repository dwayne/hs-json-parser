# JSON Parser

A JSON parser compliant with [RFC 8259](https://www.rfc-editor.org/info/rfc8259/).

## Usage

This package is distributed through its Git repository rather than Hackage. Add a `source-repository-package` stanza to `cabal.project` for it.

`cabal.project`

```
packages: .

source-repository-package
  type: git
  location: git@github.com:dwayne/hs-json-parser.git
  [tag: <hash|tag|branch>]
```

**N.B.** _Omitting the tag tracks the default branch and sacrifices reproducibility._

`example.cabal`

```
build-depends:
  json-parser
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

numFromInt :: Int -> Number
numFromInteger :: Integer -> Number
numFromScientific :: Integer -> Int -> Number

numSign :: Number -> Sign
numDigits :: Number -> Text
numFraction :: Number -> Maybe Text
numExponent :: Number -> Maybe (Sign, Text)

numToText :: Number -> Text
numToRational :: Number -> Rational

compact :: Json -> Text
pretty :: Int -> Json -> Text

writeCompact :: FilePath -> Json -> IO ()
writePretty :: FilePath -> Int -> Json -> IO ()

parse :: Text -> Either SyntaxError Json
parseFromFile :: FilePath -> IO (Either Error Json)
```
