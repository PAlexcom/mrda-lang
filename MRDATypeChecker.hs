module MRDATypeChecker where

import MRDAParser
import Control.Monad.State

data Attributes = Attributes {
    tp :: String,
    env :: [Char],
    counter :: Int
} deriving (Show)

defaultAttributes = Attributes [] 0

increaseCounter :: Attributes -> Attributes
increaseCounter attr = attr {counter = (counter attr) + 1}

------------------------------------------------------------
--------- Utilities ----------------------------------------
------------------------------------------------------------
getPrimitiveType (BasicType basicType) = case basicType of
    BasicType_bool -> "bool"
    BasicType_char -> "char"
    BasicType_float -> "float"
    BasicType_int -> "int"
    BasicType_void ->  "void"

------------------------------------------------------------
--------- Type Checker -------------------------------------
------------------------------------------------------------
checkTypesDecl :: String -> String -> Bool
checkTypes first second = first == second

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
        check_VarDeclInits tp varDeclInits
        return ()
        where
            tp = getPrimitiveType basicType
    DvarCInit TypeSpec [VarDeclInit] -> do
        return ()
    Dfun BasicType Ident [Parameter] CompStmt -> do
        return ()

check_VarDeclInits tp [] = do
    return ()

check_VarDeclInits tp [x:xs] = do
    check_VarDeclInit tp x
    check_VarDeclInits tp xs
    return ()

check_VarDeclInit tp node = case node of
    VarDeclIn ident complexRExpr -> do
        -- TODO se i tipi sono uguali butali nel ambiente
        return ()
        where
            tpRexpr = check_ComplexRExpr complexRExpr

check_ComplexRExpr node = case node of
    Simple rExpr -> do
        check_RExpr rExpr
        return ()
    Array complexRExpr -> do
        return ()

check_RExpr node = case node of
    Or RExpr RExpr
    And RExpr RExpr
    Not RExpr
    Eq RExpr RExpr
    Neq RExpr RExpr
    Lt RExpr RExpr
    LtE RExpr RExpr
    Gt RExpr RExpr
    GtE RExpr RExpr
    Add RExpr RExpr
    Sub RExpr RExpr
    Mul RExpr RExpr
    Div RExpr RExpr
    Mod RExpr RExpr
    Pow RExpr RExpr
    Neg RExpr
    Ref LExpr
    FCall FunCall
    Int Integer
    Char Char
    String String
    Float Double
    Bool Boolean
    Lexpr LExpr

