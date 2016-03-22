module MRDATypeChecker where

import MRDAParser
import Control.Monad.State
import Error

data Attributes = Attributes {
    isError :: Err String,
    env :: Enviroment,
    counter :: Int,
    levelCounter :: Int
} deriving (Show)

data Enviroment 
    = Env {
        vars :: [EnviromentElement],
        arrays :: [EnviromentElement],
        funcs :: [EnviromentElement],
        parent :: Maybe Enviroment
    }
    deriving (Show)

data EnviromentElement
    =  FuncElem {ident :: String, tp :: String, params :: [FuncParam]}
    | VarElem {ident :: String, tp :: String}
    | ArrayElem {ident :: String, tp :: String, dim :: Int}
    deriving (Show, Eq)

data FuncParam
    = ParamElem {identp :: String, tpp :: String}
    deriving (Show, Eq)

------------------------------------------------------------
--------- Utilities ----------------------------------------
------------------------------------------------------------

defaultAttributes = Attributes (Ok "") (Env [] [] [] Nothing) 0 0

increaseCounter :: Attributes -> Attributes
increaseCounter attr = attr {counter = (counter attr) + 1}

increaseLevelCounter :: Attributes -> Attributes
increaseLevelCounter attr = attr {levelCounter = (levelCounter attr) + 1}

setError :: String -> State Attributes ()
setError msg = do
    modify (\attr -> attr {isError = Bad msg})
    return ()

setParentEnv :: State Attributes ()
setParentEnv = do
    oldEnv <- gets env
    modify (\attr -> attr {env = (Env {vars = [], arrays = [], funcs = [], parent = Just oldEnv})})
    return ()

pushToEnv :: EnviromentElement -> State Attributes ()
pushToEnv envElem = case envElem of
    FuncElem _ _ _-> do
        currentEnv <- gets env
        modify (\attr -> attr {env = currentEnv {funcs = envElem : (funcs currentEnv)}})
        return ()
    ArrayElem _ _ _ -> do
        currentEnv <- gets env
        modify (\attr -> attr {env = currentEnv {arrays = envElem : (arrays currentEnv)}})
        return ()
    VarElem _ _ -> do
        currentEnv <- gets env
        modify (\attr -> attr {env = currentEnv {vars = envElem : (vars currentEnv)}})
        return ()

serializeEnvParameters :: [Parameter] -> [FuncParam]
serializeEnvParameters [] = []
serializeEnvParameters ((Param _ tp ident):params)
    = (ParamElem (getIdent ident) (getTypeSpec tp)) : serializeEnvParameters params

getIdent :: Ident -> String
getIdent (Ident ident) = ident

getBasicType :: BasicType -> String
getBasicType (BType tp) = tp

getBasicTypeSafe :: BasicType -> Err String
getBasicTypeSafe tp = Ok (getBasicType tp)

getTypeSpec :: TypeSpec -> String
getTypeSpec node = case node of
    BasTyp basicType -> getBasicType basicType
    CompType compoundType -> getCompoundType compoundType

getTypeSpecSafe :: TypeSpec -> Err String
getTypeSpecSafe node = Ok (getTypeSpec node)

getCompoundType :: CompoundType -> String
getCompoundType node = case node of
    ArrDef typeSpec integer -> checkTypesFake -- TODO
    ArrUnDef typeSpec -> checkTypesFake -- TODO
    Pointer typeSpec -> checkTypesFake -- TODO

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

checkTypesFakeSafe :: Err String
checkTypesFakeSafe = Ok "null"

checkTypesFake :: String
checkTypesFake = "null"

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
            tp = getBasicTypeSafe basicType
    DvarCInit typeSpec varDeclInits -> do
        check_VarDeclInits tp varDeclInits
        return ()
        where
            tp = getTypeSpecSafe typeSpec
    Dfun basicType ident parameters compStmt -> do
        pushToEnv $ FuncElem (getIdent ident) (getBasicType basicType) (serializeEnvParameters parameters)
        return ()

check_VarDeclInits :: Err String -> [VarDeclInit] -> State Attributes ()
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
            Ok tp1 -> pushToEnv (VarElem (getIdent ident) tp1)
        return ()
        where
            tpRexpr = check_ComplexRExpr complexRExpr

check_ComplexRExpr :: ComplexRExpr -> Err String
check_ComplexRExpr node = case node of
    Simple rExpr -> check_RExpr rExpr
    Array complexRExpr -> checkTypesFakeSafe

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
    Ref lExpr -> checkTypesFakeSafe  -- FIXME in case of any type, fixit later
    FCall funCall -> checkTypesFakeSafe -- TODO
    Int integer -> Ok "int"
    Char char -> Ok "char"
    String string -> Ok "string"
    Float double -> Ok "float"
    Bool boolean -> Ok "bool"
    Lexpr lExpr -> checkTypesFakeSafe -- TODO

