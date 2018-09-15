module ApplicativeParser where

import Data.Char
import Prelude hiding (fmap)

-- | An ambiguous parser.
newtype Parser a = P { unP :: String -> [(String, a)] }

-- | Change the result of a parser.
pmap :: (a -> b) -> Parser a -> Parser b
pmap f p = P parse
  where parse input = map (\(input', a) -> (input', f a)) (unP p input)


-- | Operator version of 'pmap'.
(<#>) :: (a -> b) -> Parser a -> Parser b
(<#>) = pmap

-- | Parse a value and replace it.
(<#) :: a -> Parser b -> Parser a
a <# p = pmap (const a) p

infixl 4 <#>
infixl 4 <#

-- | Parse a character only when a predicate matches.
predP :: (Char -> Bool) -> Parser Char
predP p = P parse 
  where parse [] = []
        parse (c:cs) | p c == True = [(cs, c)]
                     | otherwise = []

-- | Succeed only when parsing the given character.
charP :: Char -> Parser Char
charP c = predP (==c)

-- | Inject a value into an identity parser.
inject :: a -> Parser a
inject x = P $ \input -> [(input, x)]

-- | Given a parser with a function value and another parser, parse the function
-- first and then the value, return a parser which applies the function to the
-- value.
(<@>) :: Parser (a -> b) -> Parser a -> Parser b
pf <@> px = P f
  where
    f input = [ (input'', f a) | (input', f) <- unP pf input, (input'', a) <- unP px input' ]

(<@) :: Parser a -> Parser b -> Parser a
pa <@ pb = P f
  where
    f input = [ (input'', a) | (input', a) <- unP pa input, (input'', b) <- unP pb input' ]

(@>) :: Parser a -> Parser b -> Parser b
pa @> pb = P f
  where
    f input = [ (input'', b) | (input', a) <- unP pa input, (input'', b) <- unP pb input' ]

infixl 4 <@
infixl 4 @>
infixl 4 <@>

-- | Parse a whole string.
stringP :: String -> Parser String
stringP string = P $ \input -> parse input $ map charP string
  where
    parse "" _ = if string == "" then [("", "")] else []
    parse input [] = []
    parse input (p:ps) = case unP p input of
      [] -> []
      [(input', c)] -> case parse input' ps of
        [] -> [(input', [c])] 
        [(input'', cs)] -> [(input'', c:cs)]

-- | Construct a parser that never parses anything.
emptyP :: Parser a
emptyP = P $ const []

-- | Combine two parsers: When given an input, provide the results of both parser run on the input.
(<<>>) :: Parser a -> Parser a -> Parser a
pl <<>> pr = P f
  where
    f input = unP pl input ++ unP pr input


infixl 3 <<>>

-- | Apply the parser zero or more times.
many :: Parser a -> Parser [a]
many p = P parse
  where
    parse input = case unP p input of
      [] -> [(input, [])]
      [(input', a)] -> case parse input' of
        [] -> [(input', [a])]
        list -> [ (input'', a:as) | (input'', as) <- list ]


-- | Apply the parser one or more times.
some :: Parser a -> Parser [a]
some p = P parse
  where
    parse input = case unP p input of
      [] -> []
      [(input', a)] -> case parse input' of
        [] -> [(input', [a])]
        list -> [ (input'', a:as) | (input'', as) <- list ]


-- | Apply a parser and return all ambiguous results, but only those where the input was fully consumed.
runParser :: Parser a -> String -> [a]
runParser p cs = case unP p cs of
  [] -> []
  list -> [ a | (input, a) <- list, length input == 0 ]

-- | Apply a parser and only return a result, if there was only one unambiguous result with output fully consumed.
runParserUnique :: Parser a -> String -> Maybe a
runParserUnique p cs = case runParser p cs of
  [] -> Nothing
  [x] -> Just x
  (x:xs) -> Nothing

-- | Kinds of binary operators.
data BinOp = AddBO | MulBO deriving (Eq, Show)

-- | Some kind of arithmetic expression.
data Expr = ConstE Int
  | BinOpE BinOp Expr Expr
  | NegE Expr
  | ZeroE
  deriving (Eq, Show)

evalExpr :: Expr -> Int
evalExpr (ConstE n) = n
evalExpr (BinOpE AddBO expr1 expr2) = evalExpr expr1 + evalExpr expr2
evalExpr (BinOpE MulBO expr1 expr2) = evalExpr expr1 * evalExpr expr2
evalExpr (NegE expr) = -(evalExpr expr)
evalExpr ZeroE = 0

-- | Parse arithmetic expressions, with the following grammar:
--
-- expr ::= const | binOpExpr | neg | zero
-- const::= int
-- binOpExpr::= '(' expr ' ' binOp ' ' expr ')'
-- binOp::= '+' | '*'
-- neg::= '-' expr
-- zero ::= 'z'
-- 
parseExpr :: String -> Maybe Expr
parseExpr = runParserUnique expr

space = charP ' '

expr = binOpExpr <<>> negE <<>> zeroE <<>> constE

constE = (\n -> ConstE $ (read n :: Int)) <#> (some $ predP isDigit)

binOpExpr = (inject f) <@> (charP '(' @> expr) <@> (space @> binOpE <@ space) <@> (expr <@ charP ')')
  where
    f expr1 binOp expr2 = BinOpE binOp expr1 expr2

zeroE = const ZeroE <#> charP 'z'

binOpE = ((const AddBO) <#> charP '+') <<>> ((const MulBO) <#> charP '*')

negE = NegE <#> (charP '-' @> expr)