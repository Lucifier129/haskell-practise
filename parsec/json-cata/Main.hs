import System.IO
import Text.Parsec
import Control.Monad.Trans

import Parser
import Format
import RecursionSchemes

inputFilePath = "./data/input.json"
outputFilePath = "./data/output.json"

main :: IO ()
main = do
  jsonContent <- readFile inputFilePath
  case parse json "json" jsonContent of
    Left error -> print error
    Right jsonValue -> do writeFile outputFilePath $ cata formatJSON jsonValue




doPrint = do
  jsonContent <- readFile inputFilePath
  case parse json "json" jsonContent of
    Left error -> print error
    Right jsonValue -> print $ cata printJSON jsonValue

  