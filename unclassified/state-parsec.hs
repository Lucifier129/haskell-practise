import Control.Monad
import Control.Monad.Trans.State
import Control.Applicative
import Data.List

type Parser = StateT String Maybe

item :: Parser Char
item = StateT uncons

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = mfilter p item

char :: Char -> Parser Char
char c = satisfy (==c)

data T = T [T] deriving (Show)

parens :: Parser T
parens = char '(' *> many parens <* char ')' >>= (return.T)

parse = runStateT

test = parse parens "(()(()()))"

