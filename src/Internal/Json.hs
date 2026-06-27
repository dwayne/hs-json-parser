-- | A 'Json' data structure for representing JSON.
module Internal.Json
  ( Json(..)
  , Number(..), Sign(..)
  , Array
  , Object
  ) where

import Data.Text (Text)


-- | A JSON <https://www.rfc-editor.org/info/rfc8259/#section-3 value>.
data Json
  = Null
  | Boolean Bool
  | Number Number
  | String Text
  | Array Array
  | Object Object
  deriving (Eq, Show)


-- | A JSON <https://www.rfc-editor.org/info/rfc8259/#section-6 number>.
data Number
  = Num
      { numSign :: Sign                   -- ^ The sign.
      , numDigits :: Text                 -- ^ The integer part.
      , numFraction :: Maybe Text         -- ^ The fractional part, if any.
      , numExponent :: Maybe (Sign, Text) -- ^ The exponent, if any.
      }
  deriving (Eq, Show)


-- | The sign of a t'Number' or its exponent: positive ('Plus') or negative ('Minus').
data Sign
  = Plus
  | Minus
  deriving (Eq, Show)


-- | A JSON <https://www.rfc-editor.org/info/rfc8259/#section-5 array>.
type Array = [Json]


-- | A JSON <https://www.rfc-editor.org/info/rfc8259/#section-4 object>.
--
-- Represented as a list of name/value pairs, preserving insertion order
-- and allowing duplicate keys, as permitted by the RFC.
type Object = [(Text, Json)]
