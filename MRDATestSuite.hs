module MRDATestSuite where 

import System.Directory
import MRDACompiler

testFiles files = mapM_ compileFile files

main = do
    putStrLn "Run Test Suite"
    files <- getDirectoryContents "./examples"
    -- remove unwanted elements from the list e.g. [.,.., ]
    testFiles $ tail $ tail files
    return ()