module Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token

def = emptyDef {
  reservedOpNames = ["::", "..", "=", "\\", "|", "<-", "->", "@", "~", "=>"],
  reservedNames = [
    "let", "in", 
    "case", "of", 
    "if", "then", "else",
    "datatype",
    "class", "instance",
    "default", "deriving", 
    "do",
    "infix", "infixl", "infixr",
    "import", "module",
    "newtype",
    "where",
    "primitive"
  ]
  commentStart = "{-",
  commentEnd = "-}",
  commentLine = "--",
  nestedComments = True,
  identStart = letter,
  identLetter = alphaNum <|> oneOf "_'",
  opStart = opLetter def,
  opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~",
  reservedOpNames= [],
  reservedNames  = [],
  caseSensitive  = True
}

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser def

integer :: Parser Integer
integer = Token.integer lexer

float :: Parser Double
float = Token.float lexer

naturalOrFloat :: Parser (Either Integer Double)
naturalOrFloat = Token.naturalOrFloat lexer

stringLiteral :: Parser String
stringLiteral = Token.stringLiteral lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

commaSep :: Parser a -> Parser [a]
commaSep = Token.commaSep lexer

braces = Token.braces lexer
identifier = Token.identifier lexer
reserved   = Token.reserved lexer