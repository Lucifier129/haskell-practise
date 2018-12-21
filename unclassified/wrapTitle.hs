wrapTitle title layer = foldl wrap title [0..layer]
  where
    wrap title n
      | n `mod` 2 == 0 = "refute [" ++ title ++ "]"
      | n `mod` 2 == 1 = "refute {{" ++ title ++ "}}"
      | otherwise = title

test = wrapTitle "I don't know much about the DRY culture of Node.js community" 5



