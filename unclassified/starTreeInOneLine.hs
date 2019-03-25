import Data.List

mkTree height = intercalate "\n" $ map concat $ transpose [map (\i -> replicate i ' ') $ reverse [1..height], map (\i -> intercalate  " " $ map (\char -> [char]) $ replicate i '*') [1..height]]

test = putStrLn $ mkTree 5