module Format where

import Data.List

import Syntax

formatString s = "\"" ++ s ++ "\""
formatNumber n = show n
formatBoolean True =  "true"
formatBoolean False = "false"

formatJSON :: JSON -> String
formatJSON JNull = "null"
formatJSON (JNumber n) = formatNumber n
formatJSON (JString s) = formatString s
formatJSON (JBoolean b) = formatBoolean b
formatJSON (JArray xs) = (intercalate "," $ fmap formatJSON xs)
formatJSON (JObject fs) = "{" ++ (intercalate "," $ fmap formatField fs)  ++ "}"

formatField :: JSON_Field -> String
formatField (key, value) = formatString key ++ ":" ++ (formatJSON value)