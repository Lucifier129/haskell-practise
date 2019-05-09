zero = \inp -> []

result v = \inp -> [(v, inp)]

item = \inp -> case inp of
  [] -> []
  (x:xs) -> [(x, xs)]

p `bind` f = \inp -> concat [f v inp' | (v, inp') <- p inp]

p `ap` q = p `bind` \f -> q `bind` \a -> result $ f a

p `alt` q = \inp -> case p inp of
  [] -> q inp
  a -> a

sat p = item `bind` \x -> if p x then result x else zero

many p = (p `bind` \x -> many p `bind` \xs -> result $ x:xs) `alt` result []

parens p = result (\_ a _ -> a) `ap` char '(' `ap` p `ap` char ')'

data T = T [T] deriving (Show)

char c = sat (==c)

empty = parens (result [])

parser = empty `alt` parens (many parser) `bind` (result.T)

test = parser "(()(()()))"