module MRDATypeChecker where

import MRDAParser
import Control.Monad.State

tacGenerator abstractSyntaxTree = do
    print $ "Type Checking"
    print $ finalAttributes
    where
        finalAttributes = execState (check_PROGRAM abstractSyntaxTree) defaultAttributes 

data Attributes = Attributes {
    env :: [Char],
    counter :: Int
} deriving (Show)

defaultAttributes = Attributes [] 0

increaseCounter :: Attributes -> Attributes
increaseCounter attr = attr {counter = (counter attr) + 1}

------------------------------------------------------------
--------- Utilities ----------------------------------------
------------------------------------------------------------
getPrimitiveType basicType = case basicType of
    BasicType_bool -> Bool
    BasicType_char -> Char
    BasicType_float -> Float
    BasicType_int -> Int
    BasicType_void -> 

------------------------------------------------------------
--------- Type Checker -------------------------------------
------------------------------------------------------------
checkTypesDecl :: BasicType
checkTypesDecl basicType varDeclInits = 
------------------------------------------------------------
--------- Parser ABS ---------------------------------------
------------------------------------------------------------

check_Prog :: Prog -> State Attributes ()
check_Prog (Prog decls) = do
    check_Decls decls
    return ()

check_Decls :: [Decl] -> State Attributes ()
check_Decls [] = do
    return ()

check_Decls [x:xs] = do
    check_Decl x
    check_Decls xs
    return ()

check_Decl :: Decl -> State Attributes ()
check_Decl node = case node of
    DvarBInit basicType varDeclInits -> do
        checkTypesDecl basicType varDeclInits
        return ()
    DvarCInit TypeSpec [VarDeclInit] -> do
        return ()
    Dfun BasicType Ident [Parameter] CompStmt -> do
        return ()

        

