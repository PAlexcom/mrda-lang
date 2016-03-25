module Main where 

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import MRDALexer
import MRDAParser
import MRDACodeGenerator
import MRDATypeChecker
import Error
import PrettyOur

compileFile fileName = putStrLn fileName >> readFile fileName >>= compile fileName
compile fileName text = do
    putStrLn "-----------------------\n Tokens \n-----------------------"
    print tokens
    putStrLn "-----------------------\n Abstract Syntax Tree \n-----------------------"
    case abstractSyntaxTree of
        Bad msg -> do
            putStrLn ("-----------------------\n !!! Error: " ++ msg ++ " \n-----------------------")
            return ()
        Ok abst -> do
            print abst
            putStrLn "-----------------------\n Type Checking \n-----------------------"
            print typeCheckingReport
            case isTypeCheckOk of
                Ok _ -> do
                    putStrLn "-----------------------\n OK :) Type Checking \n-----------------------"
                    putStrLn "-----------------------\n TAC \n-----------------------"
                    print tacAttr
                    putStrLn "-----------------------\n TAC Source Code \n-----------------------"
                    putStrLn $ code tacAttr
                    putStrLn "-----------------------\n TAC Pretty \n-----------------------"
                    putStrLn $ stampa $ tac tacAttr
                    where
                        tacAttr = tacGenerator abst
                Bad msg -> putStrLn ("-----------------------\n!!! Error: " ++ msg ++ " \n-----------------------")
            where
                typeCheckingReport = typeChecking abst
                isTypeCheckOk = isError typeCheckingReport
    return ()
    where
        tokens = parseTokens text
        abstractSyntaxTree = pProgram tokens

main = do
  args <- getArgs
  case args of
    [] -> hGetContents stdin >>= compile ""
    fileName -> mapM_ (compileFile) fileName
