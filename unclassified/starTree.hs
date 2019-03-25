import Data.List

makeValueList value = \count -> map (const value) [1..count]

makeStarList = makeValueList "*"

makeSpaceList = makeValueList " "

makeTree producer = \height -> map producer [1..height]

makeStarTree = makeTree makeStarList

makeSpaceTree = makeTree makeSpaceList

formatStarTree starTree =  intercalate  "\n" $ map concat tree
  where
    reverseSpaceTree = reverse.makeSpaceTree $ length starTree
    flatSpaceTree = map concat reverseSpaceTree
    flatStarTree = map (intercalate  " ") starTree
    tree = transpose [flatSpaceTree, flatStarTree]

test = putStrLn $ formatStarTree (makeStarTree 5)