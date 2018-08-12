{-# LANGUAGE DeriveFunctor  #-}
module Syntax where

data JSON a = JNull
  | JString String
  | JNumber Double
  | JBoolean Bool
  | JArray [a]
  | JObject [(String, a)]
    deriving (Eq, Ord, Show, Functor)


