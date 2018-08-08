

(|>) :: (a -> b) -> (b -> c) -> (a -> c)
f |> g = g . f

switchMap :: (a -> [b]) -> [a] -> [b]
switchMap f list = list >>= f

test :: [Integer] -> [Integer]
test = 
  id
  |> map (\n -> n + 1)
  |> filter (\n -> n `mod` 2 == 1)
  |> switchMap (\n -> [n..n + 10])
  


