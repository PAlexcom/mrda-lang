module MRDACompiler where 

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import MRDALexer
import MRDAParser
-- import MRDACodeGenerator

compileFile fileName = putStrLn fileName >> readFile fileName >>= compile fileName
compile fileName text = do
    print ("Tokens")
    print (tokens)
    print ("Abstract Syntax Tree")
    print (abstractSyntaxTree)
    -- tacGenerator abstractSyntaxTree
    where
        tokens = parseTokens text
        abstractSyntaxTree = pProgram tokens

main = do
  args <- getArgs
  case args of
    [] -> hGetContents stdin >>= compile ""
    fileName -> mapM_ (compileFile) fileName
