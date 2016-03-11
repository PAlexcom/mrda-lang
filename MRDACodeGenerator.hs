module MRDACodeGenerator where

import MRDAParser
import qualified Data.Text as T

sanitizeFileName fileName = fileName ++ ".tac"

tacGenerator abstractSyntaxTree fileName = do
    print $ "Generating Code in filename " ++ (sanitizeFileName fileName)
    writeFile (sanitizeFileName fileName) $ createCode abstractSyntaxTree 

createCode abstractSyntaxTree = case abstractSyntaxTree of
    Program _ _ -> "program"
    ProgramEmpty -> ""
