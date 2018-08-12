module Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser emptyDef

integer :: Parser Integer
integer = Token.integer lexer

float :: Parser Double
float = Token.float lexer

stringLiteral :: Parser String
stringLiteral = Token.stringLiteral  lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

commaSep :: Parser a -> Parser [a]
commaSep = Token.commaSep lexer