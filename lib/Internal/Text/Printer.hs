{-# LANGUAGE OverloadedStrings #-}

module Internal.Text.Printer (pretty) where

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as TB

import Data.Char (ord)
import Data.Function ((&))
import Data.List (intersperse)
import Data.Text (StrictText)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (Builder)
import Internal.Data.Json
import Numeric (showHex)


pretty :: Int -> Json -> Text
pretty numSpaces json = TB.toLazyText $ prettyJson state json <> newline
  where
    state :: State
    state = State 0 spaces newline nameSeparator

    n :: Int
    n = max 0 numSpaces

    spaces :: Builder
    spaces = indent n " "

    newline :: Builder
    nameSeparator :: Builder
    ( newline, nameSeparator ) =
      if n == 0 then
        ( "", ":" )

      else
        ( "\n", ": " )


data State
  = State
    { sLevel :: Int
    , sSpaces :: Builder
    , sNewline :: Builder
    , sNameSeparator :: Builder
    }


prettyJson :: State -> Json -> Builder
prettyJson state json =
  case json of
    Null ->
      "null"

    Boolean b ->
      if b then "true" else "false"

    Number n ->
      prettyNumber n

    String s ->
      prettyString $ T.fromStrict s

    Array a ->
      prettyStructure state "[" "]" prettyJson a

    Object o ->
      prettyStructure state "{" "}" prettyKeyValue o


prettyNumber :: Number -> Builder
prettyNumber (Num s d mf me) =
  fromSign s <> TB.fromText d <> dotDigits <> eExponent
  where
    fromSign :: Sign -> Builder
    fromSign Plus  = ""
    fromSign Minus = "-"

    dotDigits :: Builder
    dotDigits =
      case mf of
        Nothing ->
          ""

        Just t ->
          "." <> TB.fromText t

    eExponent :: Builder
    eExponent =
      case me of
        Nothing ->
          ""

        Just (es, et) ->
          "e" <> fromSign es <> TB.fromText et


prettyString :: Text -> Builder
prettyString s =
  TB.singleton '"' <> quote s <> TB.singleton '"'


quote :: Text -> Builder
quote s =
  let
    (h, t) = T.break onEscapable s
  in
  case T.uncons t of
    Nothing ->
      TB.fromLazyText h

    Just (c, restOfT) ->
      TB.fromLazyText h <> escape c <> quote restOfT


onEscapable :: Char -> Bool
onEscapable c =
  c == '\"' || c == '\\' || c == '/' || c < '\x20'


escape :: Char -> Builder
escape '\"' = "\\\""
escape '\\' = "\\\\"
escape '/'  = "\\/"
escape '\b' = "\\b"
escape '\f' = "\\f"
escape '\n' = "\\n"
escape '\r' = "\\r"
escape '\t' = "\\t"
escape c =
  if c < '\x20' then
    let
      h = showHex (ord c) ""
    in
    TB.fromString $ "\\u" ++ replicate (4 - length h) '0' ++ h
  else
    TB.singleton c


prettyStructure :: State -> Builder -> Builder -> (State -> a -> Builder) -> [a] -> Builder
prettyStructure state@(State { sLevel = level, sSpaces = spaces, sNewline = newline }) begin end toBuilder list =
  mconcat
    [ begin
    , if null list then
        mempty

      else
        let
          nextLevel = level + 1

          nextState = state { sLevel = nextLevel }

          elements =
            list
              & map (\x -> indent nextLevel spaces <> toBuilder nextState x)
              & intersperse ("," <> newline)
              & mconcat
        in
        newline <> elements <> newline <> indent level spaces
    , end
    ]


prettyKeyValue :: State -> (StrictText, Json) -> Builder
prettyKeyValue state@(State { sNameSeparator = nameSeparator }) (name, json) =
  prettyString (T.fromStrict name) <> nameSeparator <> prettyJson state json


indent :: Int -> Builder -> Builder
indent n b = mconcat $ replicate n b
