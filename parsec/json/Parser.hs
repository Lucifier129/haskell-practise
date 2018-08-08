module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Token

import Lexer
import Syntax

json :: Parser JSON
json = try jnull <|> try jstring <|> try jnumber <|> try jboolean <|> try jarray <|> jobject

jnull :: Parser JSON
jnull = do
  s <- string "null"
  return JNull

jstring :: Parser JSON
jstring = do
  s <- stringLiteral
  return $ JString s

jinteger :: Parser JSON
jinteger = do
  n <- integer
  return $ JNumber (fromInteger n)

jfloat :: Parser JSON
jfloat = do
  n <- float
  return $ JNumber n

jnumber :: Parser JSON
jnumber = try jinteger <|> jfloat

jboolean :: Parser JSON
jboolean = do
  s <- try (string "true") <|> (string "false")
  return $ case s of
    "true" -> JBoolean True
    "false" -> JBoolean False
    _ -> error "unexpected value"

jarray :: Parser JSON
jarray = do
  spaces
  char '['
  spaces
  v <- commaSep json
  spaces
  char ']'
  spaces
  return $ JArray v

jfield :: Parser JSON_Field
jfield = do
  spaces
  key <- stringLiteral
  spaces
  char ':'
  spaces
  value <- json
  spaces
  return (key, value)

jobject :: Parser JSON
jobject = do
  spaces
  char '{'
  spaces
  fields <- commaSep jfield
  spaces
  char '}'
  spaces
  return $ JObject fields