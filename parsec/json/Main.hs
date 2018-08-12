import System.IO
import Text.Parsec
import Control.Monad.Trans

import Parser
import Format


inputFilePath = "./data/input.json"
outputFilePath = "./data/output.json"

main :: IO ()
main = do
  jsonContent <- readFile inputFilePath
  case parse json "json" jsonContent of
    Left error -> print error
    Right jsonValue -> do writeFile outputFilePath $ formatJSON jsonValue