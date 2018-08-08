_range start end list next complete
  | start <= end = _range (start + 1) end (next start list) next complete
  | otherwise = complete list

_map f source list next complete = source list (\x xs -> next (f x) xs) complete

_reverse source list next complete = source _list _next _complete 
  where
    _list = id
    _next = \x f -> f . (\list -> next x list)
    _complete = \f -> f list

toList source = source [] (\x xs -> xs ++ [x]) id
toNumber source = source 0 (\x s -> x + s) id
toString source = source "" (\n s -> s ++ show n) id

n1 = _range 1 10
n2 = _map (**2) n1
n3 = _reverse n2
list = toList n3
number = toNumber n3
string = toString n3