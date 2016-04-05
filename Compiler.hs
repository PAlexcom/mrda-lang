module Main where 

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import Lexer
import Parser
import Error
import TypeChecker
import CodeGenerator
import PrettyPrinterABS
import PrettyPrinterTAC

compileFile isDebug fileName = putStrLn fileName >> readFile fileName >>= compile isDebug fileName
compile isDebug fileName text = do
    putStrLn "-----------------------\n Tokens \n-----------------------"
    if (isDebug) then (print tokens) else do return ()
    putStrLn "-----------------------\n Abstract Syntax Tree \n-----------------------"
    case abstractSyntaxTree of
        Bad msg -> do
            putStrLn ("***********************\n" ++ msg ++ "\n***********************")
            return ()
        Ok abst -> do
            if (isDebug) then (print abst) else do return ()
            putStrLn "-----------------------\n ABS Pretty Printer \n-----------------------"
            putStrLn $ show $ pPrt $ gProgram abst
            putStrLn "-----------------------\n Type Checking \n-----------------------"
            if (isDebug) then (print typeCheckingReport) else do return ()
            case isTypeCheckOk of
                Ok _ -> do
                    putStrLn "-----------------------\n OK | Type Checking \n-----------------------"
                    putStrLn "-----------------------\n TAC \n-----------------------"
                    if (isDebug)
                        then do
                            print tacAttr
                            putStrLn "-----------------------\n TAC Source Code \n-----------------------"
                            putStrLn $ code tacAttr
                            return ()
                        else do return ()
                    putStrLn "-----------------------\n TAC Pretty Printer \n-----------------------"
                    putStrLn $ show $ prettyPrint $ tac tacAttr
                    where
                        tacAttr = tacGenerator abst
                Bad msg -> putStrLn ("***********************\n" ++ msg ++ " \n***********************")
            where
                typeCheckingReport = typeChecking abst
                isTypeCheckOk = isError typeCheckingReport
    return ()
    where
        tokens = parseTokens text
        abstractSyntaxTree = pProgram tokens

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (files)         Parse content of files verbosely."
    , "  -d (files)      Debug mode. Shows Tokens, Environment and Abstract Syntax Tree."
    ]
  exitFailure

-- Main function, entry point of the program: call e.g.: "./Compiler -d filename.sca"
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    "-d":fileName -> mapM_ (compileFile True) fileName
    fileName -> mapM_ (compileFile False) fileName
