module TypeChecker where

import Parser
import Lexer
import Control.Monad.State
import Error

data Attributes = Attributes {
    isError :: Err String,
    env :: Enviroment,
    counter :: Int,
    levelCounter :: Int,
    isLoop :: Bool
} deriving (Show)

data Enviroment 
    = Env {
        vars :: [EnviromentElement],
        funcs :: [EnviromentElement],
        parent :: Maybe Enviroment
    }
    deriving (Show)

data EnviromentElement
    = FuncElem {ident :: String, tp :: Type, params :: [Type]}
    | VarElem {ident :: String, tp :: Type, modality :: ModalityType}
    deriving (Show, Eq)

data ModalityType
    = Val
    | ValRes
    | Var
    deriving (Eq, Show, Read)    

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

-- TODO try catch
-- TODO val

------------------------------------------------------------
--------- Enviroment Utilities -----------------------------
------------------------------------------------------------

defaultAttributes = Attributes (Ok "") (Env [] [] Nothing) 0 0 False

onLoopFlag :: State Attributes ()
onLoopFlag = do
    modify (\attr -> attr {isLoop = True})
    return ()

offLoopFlag :: State Attributes ()
offLoopFlag = do
    modify (\attr -> attr {isLoop = False})
    return ()

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
    modify (\attr -> attr {env = (Env {vars = [], funcs = [], parent = Just oldEnv})})
    return ()

pushToEnv :: EnviromentElement -> State Attributes ()
pushToEnv envElem = case envElem of
    FuncElem _ _ _ -> do
        currentEnv <- gets env
        modify (\attr -> attr {env = currentEnv {funcs = envElem : (funcs currentEnv)}})
        return ()
    VarElem _ _ _ -> do
        currentEnv <- gets env
        modify (\attr -> attr {env = currentEnv {vars = envElem : (vars currentEnv)}})
        return ()

pushToEnvFuncParams :: [AbsNode] -> State Attributes ()
pushToEnvFuncParams [] = do
    return ()
pushToEnvFuncParams ((ParameterNode _ (Param modality ident tp)):params) = do
    pushToEnv $ VarElem (getIdent ident) (getType $ get_TypeSpecNode tp) (getModalityParam modality)
    pushToEnvFuncParams params
    return ()

------------------------------------------------------------
--------- Utilities ----------------------------------------
------------------------------------------------------------

isArray :: Err Type -> Bool
isArray tp = "Array" == (type2string $ getType tp)

isPointer :: Err Type -> Bool
isPointer tp = "Pointer" == (type2string $ getType tp)

getPointerType :: Err Type -> Err Type
getPointerType (Ok (TypePointer tp)) = Ok tp

getArrayType :: Err Type -> Err Type
getArrayType (Ok (TypeArray tp _)) = if (isArray (Ok tp))
    then getArrayType (Ok tp)
    else (Ok tp)

serializeEnvParameters :: [AbsNode] -> [Type]
serializeEnvParameters [] = []
serializeEnvParameters ((ParameterNode _ (Param _ _ tpNode)):params)
    = (getType $ get_TypeSpecNode tpNode) : serializeEnvParameters params

getType :: Err Type -> Type
getType (Ok tp) = tp

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
getTypeSpecSafe node = Ok (getTypeSpec node)

getTypeSpec :: TypeSpec -> Type
getTypeSpec node = case node of
    BasTyp (BasicTypeNode _ node) -> getBasicType node
    CompType (CompoundTypeNode _ node) -> getCompoundType node

getCompoundTypeSafe :: CompoundType -> Err Type
getCompoundTypeSafe node = Ok (getCompoundType node)

getCompoundType :: CompoundType -> Type
getCompoundType node = case node of
    ArrDef (TypeSpecNode _ typeSpec) int -> case int of 
        Just dim -> TypeArray (getTypeSpec typeSpec) dim
        Nothing ->  TypeArray (getTypeSpec typeSpec) 0
    Pointer (TypeSpecNode _ typeSpec) -> TypePointer (getTypeSpec typeSpec)

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

getModalityParam :: AbsNode -> ModalityType
getModalityParam (ModalityParamNode _ node) = case node of
    ModalityPEmpty -> Var
    ModalityP_val -> Val
    ModalityP_var -> Var
    ModalityP_valres -> ValRes

getModalityDecl :: AbsNode -> ModalityType
getModalityDecl (ModalityDeclNode _ node) = case node of
    ModalityD_val -> Val
    ModalityD_var -> Var

------------------------------------------------------------
--------- Type Checker -------------------------------------
------------------------------------------------------------

-- Main function, used to type check an Abstract Syntax Tree
typeChecking :: AbsNode -> Attributes
typeChecking abstractSyntaxTree = finalAttr
    where 
        finalAttr = execState (check_Prog abstractSyntaxTree) defaultAttributes

checkTypes :: Err Type -> Err Type -> Err Type
checkTypes (Ok t1) (Ok t2)  = checkTypesRaw t1 t2
checkTypes (Bad msg) _      = Bad msg
checkTypes _ (Bad msg)      = Bad msg

checkTypesRaw :: Type -> Type -> Err Type
checkTypesRaw t1 t2 
    | t1 == t2  = Ok t1
    | otherwise = getMaxType t1 t2

getMaxType :: Type -> Type -> Err Type
getMaxType TypeInt TypeFloat = Ok TypeFloat
getMaxType TypeFloat TypeInt = Ok TypeFloat
getMaxType TypeChar TypeString = Ok TypeString
getMaxType TypeString TypeChar = Ok TypeString
getMaxType _ _ = Bad "i tipi non sono compatibili"

checkBoolTypes :: Err Type -> Err Type -> Err Type
checkBoolTypes first second = case (checkTypes first second) of
    Ok tp -> if (tp == TypeBoolean)
        then Ok tp
        else Bad ("error type: must be of type 'bool'")
    Bad msg -> Bad msg

checkBoolType :: Err Type -> Err Type
checkBoolType (Ok tp) = if (tp == TypeBoolean)
    then Ok tp
    else Bad ("error type: must be of type 'bool'")

checkAritmTypes :: Err Type -> Err Type -> Err Type
checkAritmTypes first second = case (checkAritmType first) of
    Ok tp -> case (checkAritmType second) of
        Ok tp -> Ok tp
        Bad msg -> Bad (msg ++ " but found: " ++ (type2string $ getType second))
    Bad msg -> Bad (msg ++ " but found: " ++ (type2string $ getType first))

checkRelType :: Err Type -> Err Type
checkRelType first = checkRelTypes first first

checkAritmType :: Err Type -> Err Type
checkAritmType tp = if (tp == (Ok TypeInt)) || (tp == (Ok TypeFloat)) 
    then tp
    else Bad "Incorect type declaration, must be 'Int' or 'Float'"

checkRelTypes :: Err Type -> Err Type -> Err Type
checkRelTypes first second = case (checkBoolTypes first second) of
    Ok tp -> Ok tp
    Bad msg1 -> case (checkAritmTypes first second) of
        Ok tp -> Ok tp
        Bad msg2 -> Bad (msg1 ++ " or " ++ msg2)

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

isIdentInVars name ((VarElem ident tp _):vars) = if (name == ident)
    then Just tp
    else isIdentInVars name vars

getFunctionType :: FunCall -> Enviroment -> Err Type
getFunctionType (Call ident rExprsNode) env = isFunCallGood (getIdent ident) rExprsNode env

isFuncInEnv :: String -> Enviroment -> Maybe (Type, [Type])
isFuncInEnv funcName env = case match of
    Just (tp,params) -> Just (tp,params)
    Nothing -> case parentEnv of
        Just parent -> isFuncInEnv funcName parent
        Nothing -> Nothing
    where
        parentEnv = parent env
        funcsEnv = funcs env
        match = isFuncInFuncs funcName funcsEnv

isFuncInFuncs :: String -> [EnviromentElement] -> Maybe (Type,[Type])
isFuncInFuncs funcName [] = Nothing

isFuncInFuncs funcName ((FuncElem ident tp params):funcs) = if funcName == ident
    then Just (tp, params)
    else isFuncInFuncs funcName funcs

isFunCallGood :: String -> [AbsNode] -> Enviroment -> Err Type
isFunCallGood funcName rExprsNode env = 
    case (isFuncInEnv funcName env) of
        Just (tp, params) ->
            case (get_RExprsNode rExprsNode params env) of
                Nothing -> Ok tp
                Just msg -> Bad ("Error in procedure call: " ++ funcName ++ " error: " ++ msg)
        Nothing -> Bad ("Function: " ++ funcName ++ " is not declared in the scope")

getNodeInfo :: AbsNode -> String
getNodeInfo node = let (Pn line column) = (pos node) in ("Error => (line: " ++ (show line) ++ " column: " ++ (show column) ++ ")")

addToEnvPrimitiveFunctions :: State Attributes ()
addToEnvPrimitiveFunctions = do
    pushToEnv $ FuncElem "writeInt" TypeString [TypeInt]
    pushToEnv $ FuncElem "writeFloat" TypeString [TypeFloat]
    pushToEnv $ FuncElem "writeChar" TypeString [TypeChar]
    pushToEnv $ FuncElem "writeString" TypeString [TypeString]
    pushToEnv $ FuncElem "readInt" TypeInt [TypeString]
    pushToEnv $ FuncElem "readChar" TypeChar [TypeString]
    pushToEnv $ FuncElem "readFloat" TypeFloat [TypeString]
    pushToEnv $ FuncElem "readString" TypeString [TypeString]
    return ()

------------------------------------------------------------
--------- Parser ABS ---------------------------------------
------------------------------------------------------------

check_Prog :: AbsNode -> State Attributes ()
check_Prog (ProgramNode posn (Prog decls)) = do
    addToEnvPrimitiveFunctions
    check_DeclsNode decls
    return ()

check_DeclsNode :: [AbsNode] -> State Attributes ()
check_DeclsNode ((DeclNode pos x):xs) = do
    check_Decl x
    isError <- gets isError
    case isError of
        Ok _ -> do
            check_DeclsNode xs
            return()
        Bad _ -> do
            return()

check_DeclsNode [] = do
    return ()

check_Decl :: Decl -> State Attributes ()
check_Decl node = case node of
    -- Check it the declared left type is equal or consistent with the left expression type
    -- If the declaration pass the type checking verification it is inserted in the environment
    DvarBInit modalityDeclNode ident basicTypeNode complexRExprNode -> do
        env <- gets env
        case (checkTypes tp (get_ComplexRExprNode complexRExprNode env)) of
            Bad msg -> setError $ getNodeInfo complexRExprNode ++ msg
            Ok tp1 -> pushToEnv (VarElem (getIdent ident) tp1 (getModalityDecl modalityDeclNode))
        return ()
        where
            tp = get_BasicTypeNode basicTypeNode
    DvarCInit modalityDeclNode ident typeSpecNode complexRExprNode -> do
        env <- gets env
        case (checkTypes tp (get_ComplexRExprNode complexRExprNode env)) of
            Bad msg -> setError $ getNodeInfo complexRExprNode ++ msg
            Ok tp1 -> pushToEnv (VarElem (getIdent ident) tp1 (getModalityDecl modalityDeclNode))
        return ()
        where
            tp = get_TypeSpecNode typeSpecNode
    Dfun ident parametersNode basicTypeNode compStmtNode returnStmtNode -> do
        pushToEnv $ FuncElem (getIdent ident) (getType $ get_BasicTypeNode basicTypeNode) (serializeEnvParameters parametersNode)
        pushToEnvFuncParams parametersNode
        check_CompStmtNode compStmtNode
        env <- gets env
        case (let (ReturnStmtNode pos returnStmt) = returnStmtNode in (get_ReturnStmt returnStmt env)) of 
            Ok tp -> do
                case (checkTypes (get_BasicTypeNode basicTypeNode) (Ok tp)) of
                    Ok _ -> do
                        return()
                    Bad msg -> do
                        setError $ (getNodeInfo basicTypeNode) ++ "In function: " ++ (getIdent ident) ++ " declared type and returned type are not equal " ++ msg
                        return()
            Bad msg -> do
                setError $ (getNodeInfo returnStmtNode) ++ msg
                return()

check_ModalityDeclNode :: AbsNode -> State Attributes ()
check_ModalityDeclNode (ModalityDeclNode posn node) = do
    return ()

get_ReturnStmt :: ReturnStmt -> Enviroment -> Err Type
get_ReturnStmt node env = case node of
    RetExpVoid -> Ok TypeUnit
    RetExp rExpr -> get_RExprNode rExpr env

check_CompStmtNode :: AbsNode -> State Attributes ()
check_CompStmtNode (CompStmtNode _ (BlockDecl decls stmts)) = do
    check_DeclsNode decls
    check_StmtsNode stmts
    return ()

check_StmtsNode :: [AbsNode] -> State Attributes ()
check_StmtsNode (x:xs) = do
    check_StmtNode x
    isError <- gets isError
    case isError of
        Ok _ -> do
            check_StmtsNode xs
            return()
        Bad _ -> do
            return()

check_StmtsNode [] = do
    return ()

check_StmtNode :: AbsNode -> State Attributes ()
check_StmtNode (StmtNode _ node) = do
    env <- gets env
    case node of
        Comp compStmt -> do
            check_CompStmtNode compStmt
            return ()
        ProcCall (FunCallNode _ funCall) -> do
            check_FunCall funCall
            return ()
        Jmp jumpStmt -> do
            check_JumpStmtNode jumpStmt
            return ()
        Iter iterStmt -> do
            check_IterStmtNode iterStmt
            return ()
        Sel selectionStmt -> do
            check_SelectionStmtNode selectionStmt
            return ()
        Assgn lExpr (Assignment_opNode _ assignment_op) rExpr -> do
            case lExpr1 of
                Ok tp -> case rExpr1 of
                    Ok tp -> case (checkTypes lExpr1 rExpr1) of
                        -- Check if it should be an aritmetic type
                        Ok tp -> case (assignment_op) of
                            (Assign) -> do return ()
                            (AssignOp _) -> case (checkAritmTypes lExpr1 rExpr1) of
                                Ok tp -> do return ()
                                Bad msg -> setError $ msg
                        Bad msg -> setError $ msg
                    Bad msg -> setError $ (getNodeInfo rExpr) ++ msg
                Bad msg -> setError $ (getNodeInfo lExpr) ++ msg
            return ()
            where
                lExpr1 = get_LExprNode lExpr env
                rExpr1 = get_RExprNode rExpr env
        LExprStmt lExpr -> do
            case tplExpr of
                Ok tp -> do
                    return ()
                Bad msg -> do
                    setError $ (getNodeInfo lExpr) ++ msg
                    return ()
            where
                tplExpr = get_LExprNode lExpr env
        ExHandler _ -> do
            return ()

check_JumpStmtNode :: AbsNode -> State Attributes ()
check_JumpStmtNode node = case node of
    (JumpStmtNode _ Break) -> do
        isLoop <- gets isLoop
        if (isLoop)
        then do return ()
        else setError $ (getNodeInfo node) ++ "'break' statement must be used inside a loop block" 
        return ()
    (JumpStmtNode _ Continue) -> do
        isLoop <- gets isLoop
        if (isLoop)
        then do return ()
        else setError $ (getNodeInfo node) ++ "'continue' statement must be used inside a loop block" 
        return ()

check_IterStmtNode :: AbsNode -> State Attributes ()
check_IterStmtNode (IterStmtNode _ node) = do
    env <- gets env
    case node of
        While rExpr stmt -> do
            onLoopFlag
            case rExprType of
                Ok tp -> case (checkBoolType rExprType) of
                    Ok tp -> check_StmtNode stmt
                    Bad msg -> setError $ (getNodeInfo rExpr) ++ msg 
                Bad msg -> setError $ (getNodeInfo rExpr) ++ msg
            offLoopFlag
            return ()
            where
                rExprType = (get_RExprNode rExpr env)
        DoWhile stmt rExpr -> do
            onLoopFlag
            case rExprType of
                Ok tp -> case (checkBoolType rExprType) of
                    Ok tp -> check_StmtNode stmt
                    Bad msg -> setError $ (getNodeInfo rExpr) ++ msg
                Bad msg -> setError $ (getNodeInfo rExpr) ++ msg
            offLoopFlag
            return ()
            where
                rExprType = (get_RExprNode rExpr env)
        For ident rExpr1 rExpr2 stmt -> do
            onLoopFlag
            case identType of
                Ok tp -> case rExpr1Type of
                    Ok tp -> case (checkAritmType rExpr1Type) of
                        Ok tp -> case rExpr2Type of
                            Ok tp -> case (checkAritmType rExpr2Type) of
                                Ok tp -> check_StmtNode stmt
                                Bad msg -> setError $ (getNodeInfo rExpr2) ++ msg 
                            Bad msg -> setError $ (getNodeInfo rExpr2) ++ msg 
                        Bad msg -> setError $ (getNodeInfo rExpr1) ++ msg  
                    Bad msg -> setError $ (getNodeInfo rExpr1) ++ msg
                Bad msg -> setError msg
            offLoopFlag
            return ()
            where
                identType = checkIdentType (getIdent ident) env
                rExpr1Type = get_RExprNode rExpr1 env
                rExpr2Type = get_RExprNode rExpr2 env
    return ()

check_SelectionStmtNode :: AbsNode -> State Attributes ()
check_SelectionStmtNode (SelectionStmtNode _ node) = do
    env <- gets env
    case node of
        IfNoElse rExpr stmt -> case (rExprType) of
            Ok tp -> case (checkBoolType rExprType) of
                Ok tp -> check_StmtNode stmt
                Bad msg -> setError $ (getNodeInfo rExpr) ++ msg
            Bad msg -> setError $ (getNodeInfo rExpr) ++ msg
            where
                rExprType = get_RExprNode rExpr env
        IfElse rExpr stmt1 stmt2 -> case (rExprType) of
            Ok tp -> case (checkBoolType rExprType) of
                Ok tp -> do
                    check_StmtNode stmt1
                    check_StmtNode stmt2
                    return ()
                Bad msg -> setError $ (getNodeInfo rExpr) ++ msg
            Bad msg -> setError $ (getNodeInfo rExpr) ++ msg
            where
                rExprType = get_RExprNode rExpr env
    return ()

check_FunCall :: FunCall -> State Attributes ()
check_FunCall (Call ident rExprs) = do
    env <- gets env
    case (isFunCallGood funcName rExprs env) of
        Ok tp -> do
            return ()
        Bad msg -> do
            setError msg
            return ()
    where
        funcName = getIdent ident

get_RExprsNode :: [AbsNode] -> [Type] -> Enviroment -> Maybe String
get_RExprsNode [] [] _ = Nothing 
get_RExprsNode (x:xs) [] env = Just "different function arguments number"
get_RExprsNode [] (x:xs) env = Just "different function arguments number"
get_RExprsNode (rExprNode:rExprsNode) (param:params) env = case (get_RExprNode rExprNode env) of
    Ok tp -> case (checkTypesRaw tp param) of 
        Ok _ -> get_RExprsNode rExprsNode params env
        Bad _ -> Just "argument types are not equal"
    Bad msg -> Just msg

get_ComplexRExpr :: ComplexRExpr -> Enviroment -> Err Type
get_ComplexRExpr node env = case node of
    Simple rExprNode -> get_RExprNode rExprNode env
    Array (x:xs) -> case (get_ComplexRExprList xs (get_ComplexRExprNode x env) env) of
        Ok tp -> Ok (TypeArray tp ((length xs) + 1))
        Bad msg -> Bad msg

get_ComplexRExprList :: [AbsNode] -> Err Type -> Enviroment -> Err Type
get_ComplexRExprList [] tpLeft env = tpLeft
get_ComplexRExprList ((ComplexRExprNode _ complexRExpr):xs) tpLeft env = case (checkTypes tpLeft (get_ComplexRExpr complexRExpr env)) of
    Ok tp -> get_ComplexRExprList xs tpLeft env
    Bad msg -> Bad msg

get_RExpr :: RExpr -> Enviroment -> Err Type
get_RExpr node env = case node of
    OpRelation rExpr1 rExpr2 _ -> case (checkRelTypes tp1 tp2) of
        Ok tp -> Ok TypeBoolean
        Bad msg -> Bad msg
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
    Not rExpr -> checkBoolType (get_RExprNode rExpr env)
    Neg rExpr -> checkAritmType (get_RExprNode rExpr env)
    Ref lExpr -> case (lExpr1) of
        Ok tp -> Ok (TypePointer tp)
        Bad msg -> Bad msg
        where 
            lExpr1 = get_LExprNode lExpr env
    FCall funCall -> get_FunCallNode funCall env
    Int int -> Ok TypeInt
    Char char -> Ok TypeChar
    String string -> Ok TypeString
    Float double -> Ok TypeFloat
    Bool boolean -> Ok TypeBoolean
    Lexpr lExpr -> get_LExprNode lExpr env

get_LExpr :: LExpr -> Enviroment -> Err Type
get_LExpr node env = case node of
    Deref rExpr -> case (get_RExprNode rExpr env) of
        Ok (TypePointer tp) -> Ok tp
        Ok _ -> Bad "expression is not a pointer"
        Bad msg -> Bad msg
    PreIncrDecr lExpr _ -> case tpLExpr of
        Ok tp -> checkAritmType tpLExpr
        Bad msg -> Bad msg
        where tpLExpr = get_LExprNode lExpr env
    PostIncrDecr lExpr _ -> case tpLExpr of
        Ok tp -> checkAritmType tpLExpr
        Bad msg -> Bad msg
        where tpLExpr = get_LExprNode lExpr env
    BasLExpr bLExpr -> get_BLExprNode bLExpr env

get_BLExpr :: BLExpr -> Enviroment -> Err Type
get_BLExpr node env = case node of
    ArrayEl (BLExprNode _ bLExpr) (RExprNode _ rExpr) -> case (identType) of
        (Ok tp, counter) -> case (unMountArrayType (Ok tp) counter) of
            Ok tp -> Ok tp
            Bad msg -> Bad msg
        (Bad msg, _) -> Bad msg
        where
            identType = checkBLExprRExprs bLExpr rExpr 1 env
    Id ident -> checkIdentType (getIdent ident) env

unMountArrayType :: Err Type -> Int -> Err Type
unMountArrayType (Ok identType) counter = if (counter > 0)
    then if (isArray (Ok identType))
        then case (identType) of
            TypeArray tp int -> unMountArrayType (Ok tp) (counter - 1)
        else (Bad "array call, wrong dimension")
    else (Ok identType)

checkBLExprRExprs :: BLExpr -> RExpr -> Int -> Enviroment -> (Err Type, Int)
checkBLExprRExprs left right counter env = case (get_RExpr right env) of
        Ok TypeInt -> case left of
            ArrayEl (BLExprNode _ bLExpr) (RExprNode _ rExpr) -> checkBLExprRExprs bLExpr rExpr (counter+1) env
            Id ident -> (checkIdentType (getIdent ident) env, counter)
        Ok tp -> (Bad "array index must be of type Int", counter)
        Bad msg -> (Bad msg, counter)

------------------------------------------------------------
--------- Parser AbsNode -----------------------------------
------------------------------------------------------------

get_FunCallNode :: AbsNode -> Enviroment -> Err Type
get_FunCallNode (FunCallNode _ node) env = getFunctionType node env

get_BLExprNode :: AbsNode -> Enviroment -> Err Type
get_BLExprNode (BLExprNode _ node) env = get_BLExpr node env

get_LExprNode :: AbsNode -> Enviroment -> Err Type
get_LExprNode (LExprNode _ node) = get_LExpr node

get_BasicTypeNode :: AbsNode -> Err Type
get_BasicTypeNode (BasicTypeNode _ node) = getBasicTypeSafe node

get_TypeSpecNode :: AbsNode -> Err Type
get_TypeSpecNode (TypeSpecNode _ node) = getTypeSpecSafe node

get_CompoundTypeNode :: AbsNode -> Err Type
get_CompoundTypeNode (CompoundTypeNode _ node) = getCompoundTypeSafe node

get_ComplexRExprNode :: AbsNode -> Enviroment -> Err Type
get_ComplexRExprNode (ComplexRExprNode _ node) env = get_ComplexRExpr node env

get_RExprNode :: AbsNode -> Enviroment -> Err Type
get_RExprNode (RExprNode _ node) env = get_RExpr node env