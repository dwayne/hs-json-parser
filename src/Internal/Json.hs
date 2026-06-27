-- | For internal use only.
--
-- A data structure for working with JSON values in Haskell.
module Internal.Json
  ( Json(..)
  , Number(..), Sign(..)
  , Array
  , Object
  ) where

import Data.Text (Text)


-- | The representation of a JSON <https://www.rfc-editor.org/info/rfc8259/#section-3 value>.
data Json
  = Null
  | Boolean Bool
  | Number Number
  | String Text
  | Array Array
  | Object Object
  deriving (Eq, Show)


-- | The representation of a JSON <https://www.rfc-editor.org/info/rfc8259/#section-6 number>.
data Number
  = Num
      { numSign :: Sign                   -- ^ The sign.
      , numDigits :: Text                 -- ^ The integer part.
      , numFraction :: Maybe Text         -- ^ The optional fractional part.
      , numExponent :: Maybe (Sign, Text) -- ^ The optional exponent.
      }
  deriving (Eq, Show)


-- | The sign of a t'Number' or its exponent: positive ('Plus') or negative ('Minus').
data Sign
  = Plus
  | Minus
  deriving (Eq, Show)


-- | The representation of a JSON <https://www.rfc-editor.org/info/rfc8259/#section-5 array>.
type Array = [Json]


-- | The representation of a JSON <https://www.rfc-editor.org/info/rfc8259/#section-4 object>.
--
-- A list of key/value pairs, preserving insertion order and allowing duplicate keys, as permitted by the RFC.
type Object = [(Text, Json)]
