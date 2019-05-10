groupEqual [] compare = []
groupEqual [x] compare = [[x]]
groupEqual (x:xs) compare = results
  where
    (g:gs) = groupEqual xs compare
    (v:vs) = g
    results = case compare v x of
      True -> (x:g):gs
      False -> [x]:(g:gs)

source = [1, 23, 45123, 1, 221, 2, 2, 2, 2, 3, 34, 4, 4, 4, 4, 1, 1, 1, 11]
test = groupEqual source (==)

groupEqual' xs compare = foldr grouping [] xs
  where
    grouping x [] = [[x]]
    grouping x (g:gs)
      | compare (head g) x = (x:g):gs
      | otherwise = [x]:(g:gs)

test' = groupEqual' source (==)