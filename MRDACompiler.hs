module MRDACompiler where 

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import MRDALexer
import MRDAParser
import MRDACodeGenerator
import MRDATypeChecker
import Error

compileFile fileName = putStrLn fileName >> readFile fileName >>= compile fileName
compile fileName text = do
    putStrLn ("Tokens")
    print tokens
    putStrLn ("Abstract Syntax Tree")
    print abstractSyntaxTree
    case abstractSyntaxTree of
        Bad msg -> do
            putStrLn (msg)
        Ok abst -> do
            putStrLn ("Type Checking")
            print isTypeCheckOk
            putStrLn ("TAC")
            print tacAttr
            putStrLn ("TAC Source Code")
            putStrLn (code tacAttr)
            where
                isTypeCheckOk = typeChecking abst
                tacAttr = tacGenerator abst
    where
        tokens = parseTokens text
        abstractSyntaxTree = pProgram tokens

main = do
  args <- getArgs
  case args of
    [] -> hGetContents stdin >>= compile ""
    fileName -> mapM_ (compileFile) fileName
