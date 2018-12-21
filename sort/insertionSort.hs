insertionSort :: [Int] -> [Int]
insertionSort [] = []
insertionSort (x:xs) = step xs [x]
  where
    step [] sorted = sorted
    step (x:xs) sorted = step xs (insertion x sorted)
    insertion x [] = [x]
    insertion x (y:xs) = case x < y of
      True -> x:y:xs
      False -> y:(insertion x xs)