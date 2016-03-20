module MRDACompiler where 

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import MRDALexer
import MRDAParser
-- import MRDACodeGenerator
import MRDATypeChecker
import Error

compileFile fileName = putStrLn fileName >> readFile fileName >>= compile fileName
compile fileName text = do
    print ("Tokens")
    print (tokens)
    print ("Abstract Syntax Tree")
    print (abstractSyntaxTree)
    case abstractSyntaxTree of
        Bad msg -> do
            print (msg)
        Ok abst -> do
            print ("Type Checking")
            print (isTypeCheckOk)
            where
                isTypeCheckOk = typeChecking abst

    -- tacGenerator abstractSyntaxTree
    where
        tokens = parseTokens text
        abstractSyntaxTree = pProgram tokens

main = do
  args <- getArgs
  case args of
    [] -> hGetContents stdin >>= compile ""
    fileName -> mapM_ (compileFile) fileName
