module TypeChecker where

import Parser
import Control.Monad.State
import Error

-----
----- * Gestire il case di 'if' e 'while'
----- * Passaggio per costante, non può essere modificato il suo valore all'interno del blocco, non può comparire a sinistra di una dichiarazione
-----

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
        pointers :: [EnviromentElement],
        funcs :: [EnviromentElement],
        parent :: Maybe Enviroment
    }
    deriving (Show)

data EnviromentElement
    =  FuncElem {ident :: String, tp :: Type, params :: [Type]} -- TODO Bisogna modificare per aggiungere la modalità
    | VarElem {ident :: String, tp :: Type} -- TODO Bisogna modificare per aggiungere la modalità
    | ArrayElem {ident :: String, tp :: Type, dim :: Int} -- TODO Bisogna modificare per aggiungere la modalità
    | PointerElem {ident :: String, tp :: Type} -- TODO Bisogna modificare per aggiungere la modalità
    deriving (Show, Eq)

data Type 
    = TypeInt
    | TypeChar
    | TypeBoolean
    | TypeFloat
    | TypeString
    | TypeUnit
    | TypeArray Type Int
    | TypePointer Type
    | TypeError String
    deriving (Eq, Show, Read)

------------------------------------------------------------
--------- Utilities ----------------------------------------
------------------------------------------------------------

defaultAttributes = Attributes (Ok "") (Env [] [] [] [] Nothing) 0 0

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
    modify (\attr -> attr {env = (Env {vars = [], arrays = [], pointers = [], funcs = [], parent = Just oldEnv})})
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

--pushToEnvFuncParams :: [Parameter] -> State Attributes ()
--pushToEnvFuncParams [] = do
--    return ()
--pushToEnvFuncParams ((Param _ tp ident):params) = do
--    -- TODO quando vengono gestiti gli array creare la struttura dati ArrayElem
--    pushToEnv $ VarElem (getIdent ident) (getTypeSpec tp)
--    return ()

--serializeEnvParameters :: [Parameter] -> [Type]
--serializeEnvParameters [] = []
--serializeEnvParameters ((Param _ tp ident):params)
--    = (getTypeSpec tp) : serializeEnvParameters params

getIdent :: Ident -> String
getIdent (Ident ident) = ident

getBasicType :: BasicType -> Type
getBasicType (BType tp) = case tp of 
    "Boolean" -> TypeBoolean
    "Int"     -> TypeInt
    "Char"    -> TypeChar
    "Float"   -> TypeFloat
    "String"  -> TypeString
    "Unit"    -> TypeUnit

getBasicTypeSafe :: BasicType -> Err Type
getBasicTypeSafe tp = Ok (getBasicType tp)

getTypeSpecSafe :: TypeSpec -> Err Type
getTypeSpecSafe node = case node of
    BasTyp node -> get_BasicTypeNode node
    CompType node -> get_CompoundTypeNode node

getCompoundTypeSafe :: CompoundType -> Err Type
getCompoundTypeSafe node = case node of
    ArrDef typeSpec integer -> checkTypesFakeSafe -- TODO
    ArrUnDef typeSpec -> checkTypesFakeSafe -- TODO
    Pointer typeSpec -> checkTypesFakeSafe -- TODO

------------------------------------------------------------
--------- Type Checker -------------------------------------
------------------------------------------------------------

type2string :: Type -> String
type2string tp = case tp of
    TypeInt             -> "Int" 
    TypeChar            -> "Char"
    TypeBoolean         -> "Boolean"
    TypeFloat           -> "Float"
    TypeString          -> "String"
    TypeUnit            -> "Unit"
    TypeArray tp int    -> "Array"
    TypePointer tp      -> "Pointer"
    TypeError msg       -> "Error"

checkTypes :: Err Type -> Err Type -> Err Type
checkTypes (Ok t1) (Ok t2)  = checkGoodTypes t1 t2
checkTypes (Bad msg) _      = Bad msg
checkTypes _ (Bad msg)      = Bad msg

checkGoodTypes :: Type -> Type -> Err Type
checkGoodTypes t1 t2 
    | t1 == t2  = Ok t1
    | otherwise =  getMaxType t1 t2

getMaxType :: Type -> Type -> Err Type
getMaxType TypeInt TypeFloat = Ok TypeFloat
getMaxType TypeFloat TypeInt = Ok TypeFloat
getMaxType TypeChar TypeString = Ok TypeString
getMaxType TypeString TypeChar = Ok TypeString
getMaxType _ _ = Bad "i tipi non sono compatibili"


checkBoolTypes :: Err Type -> Err Type -> Err Type
checkBoolTypes first second = case check of
    Ok tp -> if (tp == TypeBoolean)
        then Ok tp
        else Bad ("error type: must be of type 'bool'")
    Bad msg -> Bad msg
    where
        check = checkTypes first second

checkAritmTypes :: Err Type -> Err Type -> Err Type
checkAritmTypes first second = case check of
    Ok tp -> if (tp == TypeInt || tp == TypeFloat)
        then Ok tp
        else Bad ("error type: must be of type 'int'")
    Bad msg -> Bad msg
    where
        check = checkTypes first second

checkAritmType :: Err Type -> Bool
checkAritmType tp = (tp == (Ok TypeInt)) || (tp == (Ok TypeFloat)) 

checkRelTypes :: Err Type -> Err Type -> Err Type
checkRelTypes first second = checkAritmTypes first second

checkIdentType :: String -> Enviroment -> Err Type
checkIdentType name env = case (isIdentInEnv name env) of
    Just tp -> Ok tp
    Nothing -> Bad ("Variable name: " ++ name ++ " is not declared in the scope")

isIdentInEnv :: String -> Enviroment -> Maybe Type
isIdentInEnv name env = case match of
    Just params -> Just params
    Nothing -> case parentEnv of
        Just parent -> isIdentInEnv name parent
        Nothing -> Nothing
    where
        parentEnv = parent env
        varsEnv = vars env
        match = isIdentInVars name varsEnv

isIdentInVars :: String -> [EnviromentElement] -> Maybe Type
isIdentInVars name [] = Nothing

isIdentInVars name ((VarElem ident tp):vars) = if name == ident
    then Just tp
    else isIdentInVars name vars

checkTypesFakeSafe :: Err Type
checkTypesFakeSafe = Ok checkTypesFake

checkTypesFake :: Type
checkTypesFake = TypeError "Type fake"

--getFunctionType :: FunCall -> Enviroment -> Err Type
--getFunctionType (Call ident rExprs) env = isFunCallGood (getIdent ident) rExprs env


--isFuncInEnv :: String -> Enviroment -> Maybe (Type, [Type])
--isFuncInEnv funcName env = case match of
--    Just (tp,params) -> Just (tp,params)
--    Nothing -> case parentEnv of
--        Just parent -> isFuncInEnv funcName parent
--        Nothing -> Nothing
--    where
--        parentEnv = parent env
--        funcsEnv = funcs env
--        match = isFuncInFuncs funcName funcsEnv

--isFuncInFuncs :: String -> [EnviromentElement] -> Maybe (Type,[Type])
--isFuncInFuncs funcName [] = Nothing

--isFuncInFuncs funcName ((FuncElem ident tp params):funcs) = if funcName == ident
--    then Just (tp, params)
--    else isFuncInFuncs funcName funcs

--isFunCallGood :: String -> [RExpr] -> Enviroment -> Err Type
--isFunCallGood funcName rExprs env = 
--    case (isFuncInEnv funcName env) of
--        Just (tp, params) ->
--            case (check_RExprs rExprs params env) of
--                Nothing -> Ok tp
--                Just msg -> Bad ("Error in procedure call: " ++ funcName ++ " error: " ++ msg)
--        Nothing -> Bad ("Function: " ++ funcName ++ " is not declared in the scope")

-- Main function, used to type check an Abstract Syntax Tree
typeChecking :: AbsNode -> Attributes
typeChecking abstractSyntaxTree = finalAttr
    where 
        finalAttr = execState (check_Prog abstractSyntaxTree) defaultAttributes

------------------------------------------------------------
--------- Parser ABS ---------------------------------------
------------------------------------------------------------

check_Prog :: AbsNode -> State Attributes ()
check_Prog (ProgramNode posn (Prog decls)) = do
    check_Decls decls
    return ()

check_Decls :: [AbsNode] -> State Attributes ()
check_Decls ((DeclNode pos x):xs) = do
    check_Decl x
    isError <- gets isError
    case isError of
        Ok _ -> do
            check_Decls xs
            return()
        Bad _ -> do
            return()

check_Decls [] = do
    return ()

check_Decl :: Decl -> State Attributes ()
check_Decl node = case node of
    -- Check it the declared left type is equal or consistent with the left expression type
    -- If the declaration pass the type checking verification it is inserted in the environment
    DvarBInit modalityDeclNode ident basicTypeNode complexRExprNode -> do
        env <- gets env
        case (checkTypes tp (get_ComplexRExprNode complexRExprNode env)) of
            Bad msg -> setError msg
            Ok tp1 -> pushToEnv (VarElem (getIdent ident) tp1)
        return ()
        where
            tp = get_BasicTypeNode basicTypeNode
    -- TODO handle array and pointer cases
    DvarCInit modalityDeclNode ident typeSpecNode complexRExprNode -> do
        env <- gets env
        case (checkTypes tp (get_ComplexRExprNode complexRExprNode env)) of
            Bad msg -> setError msg
            Ok tp1 -> pushToEnv (VarElem (getIdent ident) tp1)
        return ()
        where
            tp = get_TypeSpecNode typeSpecNode
    Dfun ident parametersNode basicTypeNode compStmtNode returnStmtNode -> do
        --pushToEnv $ FuncElem (getIdent ident) (getBasicType basicType) (serializeEnvParameters parameters)
        --pushToEnvFuncParams parameters
        --check_CompStmt compStmt
        --env <- gets env
        --case (check_ReturnStmt returnStmt env) of 
        --    Ok tp -> do
        --        case (checkTypes (getBasicTypeSafe basicType) (Ok tp)) of
        --            Ok _ -> do
        --                return()
        --            Bad msg -> do
        --                setError $ "In function: " ++ (getIdent ident) ++ " declared type and returned type are not equal"
        --                return()
        --    Bad msg -> do
        --        setError msg
        --        return()
        return()

check_ModalityDeclNode :: AbsNode -> State Attributes ()
check_ModalityDeclNode (ModalityDeclNode posn node) = do
    return ()

--check_ReturnStmt :: ReturnStmt -> Enviroment -> Err Type
--check_ReturnStmt node env = case node of
--    RetExpVoid -> Ok TypeUnit
--    RetExp rExpr -> check_RExpr rExpr env

--check_CompStmt :: CompStmt -> State Attributes ()
--check_CompStmt (BlockDecl decls stmts) = do
--    check_Decls decls
--    check_Stmts stmts
--    return ()

--check_Stmts :: [Stmt] -> State Attributes ()
--check_Stmts (x:xs) = do
--    check_Stmt x
--    isError <- gets isError
--    case isError of
--        Ok _ -> do
--            check_Stmts xs
--            return()
--        Bad _ -> do
--            return()
--    return ()

--check_Stmts [] = do
--    return ()

--check_Stmt :: Stmt -> State Attributes ()
--check_Stmt node = do
--    env <- gets env
--    case node of
--        Comp compStmt -> do
--            check_CompStmt compStmt
--            return ()
--        ProcCall funCall -> do
--            check_FunCall funCall
--            return ()
--        Jmp jumpStmt -> do
--            return ()
--        Iter iterStmt -> do
--            return ()
--        Sel selectionStmt -> do
--            return ()
--        Assgn lExpr assignment_op rExpr -> do
--            return ()
--        LExprStmt lExpr -> do
--            case tplExpr of
--                Ok tp -> do
--                    return ()
--                Bad msg -> do
--                    setError msg
--                    return ()
--            return ()
--            where
--                tplExpr = check_LExpr lExpr env
--    return ()

--check_FunCall :: FunCall -> State Attributes ()
--check_FunCall (Call ident rExprs) = do
--    env <- gets env
--    case (isFunCallGood funcName rExprs env) of
--        Ok tp -> do
--            return ()
--        Bad msg -> do
--            setError msg
--            return ()
--    return ()
--    where
--        funcName = getIdent ident

--check_RExprs :: [RExpr] -> [Type] -> Enviroment -> Maybe String
--check_RExprs [] [] _ = Nothing 
--check_RExprs (x:xs) [] env = Just "different function arguments number"
--check_RExprs [] (x:xs) env = Just "different function arguments number"
--check_RExprs (rExpr:rExprs) (param:params) env = case (check_RExpr rExpr env) of
--    Ok tp -> case (checkGoodTypes tp param) of 
--                Ok _    -> check_RExprs rExprs params env
--                Bad _   -> Just "argument types are not equal"
--    Bad msg -> Just msg

get_ComplexRExpr :: ComplexRExpr -> Enviroment -> Err Type
get_ComplexRExpr node env = case node of
    Simple rExprNode -> get_RExprNode rExprNode env
    Array complexRExprNode -> checkTypesFakeSafe

get_RExpr :: RExpr -> Enviroment -> Err Type
get_RExpr node env = case node of
    OpRelation rExpr1 rExpr2 _ -> checkRelTypes tp1 tp2
        where
            tp1 = get_RExprNode rExpr1 env
            tp2 = get_RExprNode rExpr2 env
    OpAritm rExpr1 rExpr2 _ -> checkAritmTypes tp1 tp2
        where
            tp1 = get_RExprNode rExpr1 env
            tp2 = get_RExprNode rExpr2 env
    OpBoolean rExpr1 rExpr2 _ -> checkBoolTypes tp1 tp2
        where
            tp1 = get_RExprNode rExpr1 env
            tp2 = get_RExprNode rExpr2 env
    Not rExpr -> get_RExprNode rExpr env
    Neg rExpr -> get_RExprNode rExpr env
    Ref lExpr -> get_LExprNode lExpr env
    --FCall funCall -> getFunctionType funCall env
    Int integer -> Ok TypeInt
    Char char -> Ok TypeChar
    String string -> Ok TypeString
    Float double -> Ok TypeFloat
    Bool boolean -> Ok TypeBoolean
    Lexpr lExpr -> get_LExprNode lExpr env

get_LExpr :: LExpr -> Enviroment -> Err Type
get_LExpr node env = case node of
    Deref rExpr -> case tpRExpr of
        Ok tp -> if checkAritmType tpRExpr
            then Ok tp
            else Bad ("Deref expressions must be of type int or float, but: " ++ (type2string tp) ++ " found")
        Bad msg -> Bad msg
        where tpRExpr = get_RExprNode rExpr env
    PreInc lExpr -> case tpLExpr of
        Ok tp -> if checkAritmType tpLExpr
            then Ok tp
            else Bad ("PreInc expressions must be of type int or float, but: " ++ (type2string tp) ++ " found")
        Bad msg -> Bad msg
        where tpLExpr = get_LExprNode lExpr env
    PreDecr lExpr -> case tpLExpr of
        Ok tp -> if checkAritmType tpLExpr
            then Ok tp
            else Bad ("PreInc expressions must be of type int or float, but: " ++ (type2string tp) ++ " found")
        Bad msg -> Bad msg
        where tpLExpr = get_LExprNode lExpr env
    PostInc lExpr -> case tpLExpr of
        Ok tp -> if checkAritmType tpLExpr
            then Ok tp
            else Bad ("PosInc expressions must be of type int or float, but: " ++ (type2string tp) ++ " found")
        Bad msg -> Bad msg
        where tpLExpr = get_LExprNode lExpr env
    PostDecr lExpr -> case tpLExpr of
        Ok tp -> if checkAritmType tpLExpr
            then Ok tp
            else Bad ("PostDecr expressions must be of type int or float, but: " ++ (type2string tp) ++ " found")
        Bad msg -> Bad msg
        where tpLExpr = get_LExprNode lExpr env
    BasLExpr bLExpr -> get_BLExprNode bLExpr env

check_BLExpr :: BLExpr -> Enviroment -> Err Type
check_BLExpr node env = case node of
    ArrayEl bLExpr rExpr -> checkTypesFakeSafe -- TODO
    Id ident -> checkIdentType (getIdent ident) env

get_BLExprNode :: AbsNode -> Enviroment -> Err Type
get_BLExprNode (BLExprNode _ node) env = check_BLExpr node env

get_LExprNode :: AbsNode -> Enviroment -> Err Type
get_LExprNode (LExprNode _ node) = get_LExpr node

get_BasicTypeNode :: AbsNode -> Err Type
get_BasicTypeNode (BasicTypeNode posn node) = getBasicTypeSafe node

get_TypeSpecNode :: AbsNode -> Err Type
get_TypeSpecNode (TypeSpecNode _ node) = getTypeSpecSafe node

get_CompoundTypeNode :: AbsNode -> Err Type
get_CompoundTypeNode (CompoundTypeNode _ node) = getCompoundTypeSafe node

get_ComplexRExprNode :: AbsNode -> Enviroment -> Err Type
get_ComplexRExprNode (ComplexRExprNode _ node) env = get_ComplexRExpr node env

get_RExprNode :: AbsNode -> Enviroment -> Err Type
get_RExprNode (RExprNode _ node) env = get_RExpr node env