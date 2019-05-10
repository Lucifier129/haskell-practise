import Control.Monad
import Control.Monad.Trans.State
import Control.Applicative
import Data.List

type Parser = StateT String Maybe

item :: Parser Char
item = StateT uncons

char :: Char -> Parser Char
char c = mfilter (==c) item

data T = T [T] deriving (Show)

parens :: Parser T
parens = char '(' *> many parens <* char ')' >>= (return.T)

parse = runStateT

test = parse parens "(()(()()))"

