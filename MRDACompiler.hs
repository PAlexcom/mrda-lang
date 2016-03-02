import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import MRDALexer
import MRDAParser

runFile f = putStrLn f >> readFile f >>= run
run s = do
    print ("Tokens")
    print (tokens)
    print ("Derivation Tree")
    print (derivationTree)
    where
        tokens = alexScanTokens s
        derivationTree = parseTokens tokens

main = do
  args <- getArgs
  case args of
    [] -> hGetContents stdin >>= run
    fs -> mapM_ (runFile) fs