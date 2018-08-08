module Syntax where

type JSON_Field = (String, JSON)
data JSON = JNull
  | JString String
  | JNumber Double
  | JBoolean Bool
  | JArray [JSON]
  | JObject [JSON_Field]
    deriving (Eq, Ord, Show)