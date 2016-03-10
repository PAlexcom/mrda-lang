import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import MRDALexer
import MRDAParser
import MRDACodeGenerator

compileFile fileName = putStrLn fileName >> readFile fileName >>= compile
compile text = do
    print ("Tokens")
    print (tokens)
    print ("Abstract Syntax Tree")
    print (abstractSyntaxTree)
    --print ("Code")
    --print (generatedCode)
    where
        tokens = alexScanTokens text
        abstractSyntaxTree = parseTokens tokens
        --generatedCode = tacGenerator abstractSyntaxTree

main = do
  args <- getArgs
  case args of
    [] -> hGetContents stdin >>= compile
    fileName -> mapM_ (compileFile) fileName