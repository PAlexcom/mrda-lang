module MRDACodeGenerator where

import MRDAParser
import qualified Data.Text as T

sanitizeFileName fileName = fileName ++ ".tac"

tacGenerator abstractSyntaxTree fileName = do
    print $ "Generating Code in filename " ++ (sanitizeFileName fileName)
    writeFile (sanitizeFileName fileName) $ code_PROGRAM abstractSyntaxTree 

code_PROGRAM node = case node of 
    Program block program -> (code_BLOCK block) ++ (code_PROGRAM program)
    ProgramEmpty -> ""

code_BLOCK node = case node of
    BlockDecl declaration -> "declaration"
    BlockIf blockif -> (code_BLOCKIF blockif)

code_BLOCKIF node = case node of
    StatementIfElse expression block1 block2 -> "expression" ++ (code_BLOCK block1) ++ (code_BLOCK block2)
    StatementIf expression block -> "expression" ++ (code_BLOCK block)