module Format where

import Data.List

import Syntax

formatString s = "\"" ++ s ++ "\""
formatNumber n = show n
formatBoolean b = if b then "true" else "false"

formatJSON :: JSON -> String
formatJSON json = render json ""
  where
    render JNull result = result ++ "null"
    render (JNumber n) result = result ++ formatNumber n
    render (JString s) result = result ++ formatString s
    render (JBoolean b) result = result ++ formatBoolean b
    render (JArray xs) result = result ++ (intercalate "," $ fmap formatJSON xs)
    render (JObject fs) result = result ++ "{" ++ (intercalate "," $ fmap formatField fs)  ++ "}"
    formatField (key, value) = formatString key ++ ":" ++ (formatJSON value)