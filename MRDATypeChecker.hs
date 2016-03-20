module MRDATypeChecker where

import MRDAParser
import Control.Monad.State
import Error

data Attributes = Attributes {
    isError :: Err String,
    env :: [(String, String, Int)],
    counter :: Int,
    levelCounter :: Int
} deriving (Show)

------------------------------------------------------------
--------- Utilities ----------------------------------------
------------------------------------------------------------
getPrimitiveType :: BasicType -> Err String
getPrimitiveType (BType basicType) = Ok basicType

defaultAttributes = Attributes (Ok "") [] 0 0

increaseCounter :: Attributes -> Attributes
increaseCounter attr = attr {counter = (counter attr) + 1}

increaseLevelCounter :: Attributes -> Attributes
increaseLevelCounter attr = attr {levelCounter = (levelCounter attr) + 1}

setError :: String -> State Attributes ()
setError msg = do
    modify (\attr -> attr {isError = Bad msg})
    return ()

pushToEnv :: Ident -> Err String -> State Attributes ()
pushToEnv (Ident ident) (Ok tp) = do
    modify (\attr -> attr {env = (ident, tp, levelCounter attr) : (env attr)})
    return ()

------------------------------------------------------------
--------- Type Checker -------------------------------------
------------------------------------------------------------
checkTypes :: Err String -> Err String -> Err String
checkTypes first second = case first of
    Ok tp -> case second of
        Ok tp2 -> if tp == tp2
            then Ok tp
            else Bad ("error type: " ++ tp ++ " different from type: " ++ tp2)
        Bad msg2 -> Bad msg2
    Bad msg -> Bad msg

checkBoolTypes :: Err String -> Err String -> Err String
checkBoolTypes first second = case check of
    Ok tp -> if (tp == "bool")
        then Ok tp
        else Bad ("error type: " ++ tp ++ "must be of type 'bool'")
    Bad msg -> Bad msg
    where
        check = checkTypes first second

checkAritmTypes :: Err String -> Err String -> Err String
checkAritmTypes first second = case check of
    Ok tp -> if (tp == "int" || tp == "float")
        then Ok tp
        else Bad ("error type: " ++ tp ++ "must be of type 'bool'")
    Bad msg -> Bad msg
    where
        check = checkTypes first second

checkRelTypes :: Err String -> Err String -> Err String
checkRelTypes first second = checkAritmTypes first second

checkTypesFake :: Err String
checkTypesFake = Ok "null"

------------------------------------------------------------
--------- Parser ABS ---------------------------------------
------------------------------------------------------------
typeChecking :: Program -> Attributes
typeChecking abstractSyntaxTree = finalAttr
    where 
        finalAttr = execState (check_Prog abstractSyntaxTree) defaultAttributes

check_Prog :: Program -> State Attributes ()
check_Prog (Prog decls) = do
    check_Decls decls
    return ()

check_Decls :: [Decl] -> State Attributes ()
check_Decls (x:xs) = do
    check_Decl x
    check_Decls xs
    return ()

check_Decls [] = do
    return ()

check_Decl :: Decl -> State Attributes ()
check_Decl node = case node of
    DvarBInit basicType varDeclInits -> do
        check_VarDeclInits tp varDeclInits
        return ()
        where
            tp = getPrimitiveType basicType
    DvarCInit typeSpec varDeclInits -> do
        check_VarDeclInits tp varDeclInits
        return ()
        where
            tp = check_TypeSpec typeSpec
    Dfun basicType ident parameters compStmt -> do
        return ()

check_VarDeclInits tp [] = do
    return ()

check_VarDeclInits tp (x:xs) = do
    check_VarDeclInit tp x
    check_VarDeclInits tp xs
    return ()

check_VarDeclInit :: Err String -> VarDeclInit -> State Attributes ()
check_VarDeclInit tp node = case node of
    VarDeclIn ident complexRExpr -> do
        case (checkTypes tp tpRexpr) of
            Bad msg -> setError msg
            Ok _ -> pushToEnv ident tp
        return ()
        where
            tpRexpr = check_ComplexRExpr complexRExpr

check_ComplexRExpr :: ComplexRExpr -> Err String
check_ComplexRExpr node = case node of
    Simple rExpr -> check_RExpr rExpr
    Array complexRExpr -> checkTypesFake

check_RExpr :: RExpr -> Err String
check_RExpr node = case node of
    OpRelation rExpr1 rExpr2 _ -> checkRelTypes tp1 tp2
        where
            tp1 = check_RExpr rExpr1
            tp2 = check_RExpr rExpr2
    OpAritm rExpr1 rExpr2 _ -> checkAritmTypes tp1 tp2
        where
            tp1 = check_RExpr rExpr1
            tp2 = check_RExpr rExpr2
    OpBoolean rExpr1 rExpr2 _ -> checkBoolTypes tp1 tp2
        where
            tp1 = check_RExpr rExpr1
            tp2 = check_RExpr rExpr2
    Not rExpr -> check_RExpr rExpr
    Neg rExpr -> check_RExpr rExpr
    Ref lExpr -> checkTypesFake  -- FIXME in case of any type, fixit later
    FCall funCall -> checkTypesFake -- TODO
    Int integer -> Ok "int"
    Char char -> Ok "char"
    String string -> Ok "string"
    Float double -> Ok "float"
    Bool boolean -> Ok "bool"
    Lexpr lExpr -> checkTypesFake -- TODO

check_TypeSpec :: TypeSpec -> Err String
check_TypeSpec node = case node of
    BasTyp basicType -> getPrimitiveType basicType
    CompType compoundType -> check_CompoundType compoundType

check_CompoundType :: CompoundType -> Err String
check_CompoundType node = case node of
    ArrDef typeSpec integer -> checkTypesFake -- TODO
    ArrUnDef typeSpec -> checkTypesFake -- TODO
    Pointer typeSpec -> checkTypesFake -- TODO

