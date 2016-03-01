import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import MRDALexer

runFile f = putStrLn f >> readFile f >>= run
run s = print (alexScanTokens s)

main = do
  args <- getArgs
  case args of
    [] -> hGetContents stdin >>= run
    fs -> mapM_ (runFile) fs