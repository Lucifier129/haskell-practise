module Format where

import Data.List
import Data.Monoid
import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as Pretty

import RecursionSchemes
import Syntax

formatJSON :: Algebra JSON String
formatJSON JNull = "null"
formatJSON (JNumber n) = formatNumber n
formatJSON (JString s) = formatString s
formatJSON (JBoolean b) = formatBoolean b
formatJSON (JArray xs) = intercalate "," xs
formatJSON (JObject fs) = "{" ++ (intercalate "," $ fmap formatField fs)  ++ "}"

formatField (key, value) = formatString key ++ ":" ++ value
formatString s = "\"" ++ s ++ "\""
formatNumber n = show n
formatBoolean True =  "true"
formatBoolean False = "false"

printJSON :: Algebra JSON Doc
printJSON JNull = Pretty.text "null"
printJSON (JNumber n) = printNumber n
printJSON (JString s) = printString s
printJSON (JBoolean b) = printBoolean b
printJSON (JArray xs) = Pretty.cat $ Pretty.punctuate Pretty.comma xs
printJSON (JObject fs) = Pretty.lbrace <> (Pretty.cat $ Pretty.punctuate Pretty.comma $ fmap printField fs)  <> Pretty.rbrace

printField (key, value) = printString key <> Pretty.colon <> Pretty.space <> value
printString s = Pretty.text $ "\"" ++ s ++ "\""
printNumber n = Pretty.double n
printBoolean True =  Pretty.text "true"
printBoolean False = Pretty.text "false"
