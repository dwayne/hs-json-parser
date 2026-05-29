# JSON Parser

A JSON parser compliant with [RFC 8259](https://www.rfc-editor.org/info/rfc8259/).

## Usage

`cabal.project`

```cabal
packages: .

source-repository-package
  type: git
  location: git@github.com:dwayne/hs-json-parser.git
  tag: f476ddd8538b2c536ddd65422cbcdfbaa39f24fa
```

`example.cabal`

```cabal
build-depends:
  json-parser
```

## Public API

```haskell
data Json
  = JsonNull
  | JsonBoolean Bool
  | JsonNumber Number
  | JsonString Text
  | JsonArray Array
  | JsonObject Object

data Number
  = Number
      { numSign :: Sign
      , numDigits :: Text
      , numFraction :: Maybe Text
      , numExponent :: Maybe (Sign, Text)
      }

data Sign
  = Plus
  | Minus

type Array = [Json]

type Object = [(Text, Json)]

data Error
  = EncodingError UnicodeException
  | SyntaxError SyntaxError

type SyntaxError = ParseErrorBundle Text Void

numFromInt :: Int -> Number
numFromInteger :: Integer -> Number
numToText :: Number -> Text
numToRational :: Number -> Rational

parse :: Text -> Either SyntaxError Json
parseFromFile :: FilePath -> IO (Either Error Json)
```
