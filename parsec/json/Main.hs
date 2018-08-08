import System.IO
import Text.Parsec
import Control.Monad.Trans

import Parser
import Format

main :: IO ()
main = do
  jsonContent <- readFile "./data/input.json"
  case parse json "json" jsonContent of
    Left error -> print error
    Right jsonValue -> do writeFile "./data/output.json" $ formatJSON jsonValue