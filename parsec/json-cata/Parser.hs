module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Token

import RecursionSchemes
import Lexer
import Syntax

json :: Parser (Term JSON)
json = try jnull <|> try jstring <|> try jnumber <|> try jboolean <|> try jarray <|> jobject

jnull :: Parser (Term JSON)
jnull = do
  s <- string "null"
  return $ In JNull

jstring :: Parser (Term JSON)
jstring = do
  s <- stringLiteral
  return $ In $ JString s

jinteger :: Parser (Term JSON)
jinteger = do
  n <- integer
  return $ In $ JNumber (fromInteger n)

jfloat :: Parser (Term JSON)
jfloat = do
  n <- float
  return $ In $ JNumber n

jnumber :: Parser (Term JSON)
jnumber = try jinteger <|> jfloat

jboolean :: Parser (Term JSON)
jboolean = do
  s <- try (string "true") <|> (string "false")
  return $ case s of
    "true" -> In $ JBoolean True
    "false" -> In $ JBoolean False

jarray :: Parser (Term JSON)
jarray = do
  spaces
  char '['
  spaces
  v <- commaSep json
  spaces
  char ']'
  spaces
  return $ In $ JArray v

jfield :: Parser (String, Term JSON)
jfield = do
  spaces
  key <- stringLiteral
  spaces
  char ':'
  spaces
  value <- json
  spaces
  return (key, value)

jobject :: Parser (Term JSON)
jobject = do
  spaces
  char '{'
  spaces
  fields <- commaSep jfield
  spaces
  char '}'
  spaces
  return $ In $ JObject fields