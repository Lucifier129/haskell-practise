import Control.Monad
import Control.Applicative

newtype Parser a = Parser { runP :: String -> [(a, String)] }

instance Functor Parser where
  fmap f p = Parser $ \inp -> [(f x, inp') | (x, inp') <- runP p inp]

instance Applicative Parser where
  pure a = Parser $ \inp -> [(a, inp)]
  p <*> q = Parser $ \inp -> [(f a, inp'') | (f, inp') <- runP p inp, (a, inp'') <- runP q inp']

instance Alternative Parser where
  empty = Parser $ \inp -> []
  p <|> q = Parser $ \inp -> case runP p inp of
    [] -> runP q inp
    a -> a

item = Parser $ \inp -> if length inp == 0 then [] else [(head inp, tail inp)]
  
sat p = Parser $ \inp -> [(a, inp') | (a, inp') <- runP item inp, p a]

data T = T [T] deriving (Show)

char c = sat (==c)

parens p = char '(' *> p <* char ')'

parser = pure T <*> (parens $ many parser)

test = runP parser "(()(()()))"

