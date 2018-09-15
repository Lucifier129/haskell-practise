quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort [x] = [x]
quickSort (x:xs) = left ++ [x] ++ right
  where
    left = quickSort $ filter (<x) xs
    right = quickSort $ filter (>=x) xs

