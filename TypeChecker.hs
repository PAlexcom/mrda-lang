module TypeChecker where

import Parser
import Lexer
import Control.Monad.State
import Error

data Attributes = Attributes {
    isError :: Err String,      -- Traccia di eventuali errori presenti nel sorgente
    env :: Enviroment,          -- Verifica tenendo traccia dello scope in maniera gerarchica
    isLoop :: Bool              -- Per tener traccia se si è in un blocco di iterazione o meno
} deriving (Show)

data Enviroment 
    = Env {
        vars :: [EnviromentElement],    -- Variabili presenti dentro lo scope attuale
        funcs :: [EnviromentElement],   -- Funzioni presenti dentro lo scope attuale
        parent :: Maybe Enviroment      -- Lo scope parent, da utilizzare all'uscita del livello attuale
    }
    deriving (Show)

data EnviromentElement
    = FuncElem {ident :: String, tp :: Type, params :: [Type]}
    | VarElem {ident :: String, tp :: Type, modality :: ModalityType}
    deriving (Show, Eq)

-- Le varie modalità di passaggio dei parametri
data ModalityType
    = Val
    | ValRes
    | Var
    deriving (Eq, Show, Read)    

-- I tipi primitivi possibili
data Type 
    = TypeInt
    | TypeChar
    | TypeBoolean
    | TypeFloat
    | TypeString
    | TypeUnit
    | TypeArray Type Int
    | TypePointer Type
    deriving (Eq, Show, Read)

------------------------------------------------------------
--------- Enviroment Utilities -----------------------------
------------------------------------------------------------

defaultAttributes = Attributes (Ok "") (Env [] [] Nothing) False

-- Da abilitare nel momento in cui si entra in uno statement di iterazione
onLoopFlag :: State Attributes ()
onLoopFlag = do
    modify (\attr -> attr {isLoop = True})
    return ()

-- Da disabilitare nel momento in cui si esce in uno statement di iterazione
offLoopFlag :: State Attributes ()
offLoopFlag = do
    modify (\attr -> attr {isLoop = False})
    return ()

-- Imposta un messaggio di errore riscontratto durante l'analisi, il programma si ferma
-- nel momento in cui persiste un'errrore
setError :: String -> State Attributes ()
setError msg = do
    modify (\attr -> attr {isError = Bad msg})
    return ()

setNewEnv :: State Attributes ()
setNewEnv = do
    currentEnv <- gets env
    modify (\attr -> attr {env = (Env {vars = [], funcs = [], parent = Just currentEnv})})
    return ()

setOldEnv :: State Attributes ()
setOldEnv = do
    currentEnv <- gets env
    case (parent currentEnv) of
        Just parentEnv -> modify (\attr -> attr {env = parentEnv})
    return ()

-- Aggiunte nel Enviroment (scope attuale) una nuova funzione o variabile
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

-- Inserisce nel Enviroment (scope attuale) i nomi dei parametri formali delle variabili
-- definite dalla funzione
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
    ArrDef (TypeSpecNode _ typeSpec) dim -> TypeArray (getTypeSpec typeSpec) dim
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

-- Controlla che i due tipi siano coerenti
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
--getMaxType TypeChar TypeString = Ok TypeString
--getMaxType TypeString TypeChar = Ok TypeString
getMaxType tp1 tp2 = Bad ("i tipi ('" ++ (type2string tp1) ++ "' e '" ++ (type2string tp2) ++ "') non sono compatibili.")

checkBoolTypes :: Err Type -> Err Type -> Err Type
checkBoolTypes first second = case (checkTypes first second) of
    Ok tp -> if (tp == TypeBoolean)
        then Ok tp
        else Bad ("error type: must be of type 'bool', but found " ++ (type2string tp))
    Bad msg -> Bad msg

-- Controlla che i due tipi siano utilizzabili su operazioni booleane
checkBoolType :: Err Type -> Err Type
checkBoolType (Ok tp) = if (tp == TypeBoolean)
    then Ok tp
    else Bad ("error type: must be of type 'bool', but found " ++ (type2string tp))

-- Controlla che i due tipi siano utilizzabili su operazioni aritmetiche
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

-- Esegue il controlle per vedere se la modalità della variabile in esame
-- e di tipo val (cioè constant)
isIdentVal :: String -> Enviroment -> Bool
isIdentVal name env = case match of
    Just (VarElem ident tp modality) -> (Val == modality)
    Nothing -> case parentEnv of
        Just parent -> isIdentVal name parent
        Nothing -> False
    where
        parentEnv = parent env
        varsEnv = vars env
        match = isIdentInVars name varsEnv

-- Controlla se l'identificativo fornite è presente nello scope (attuale o se è stato definito in un parent)
isIdentInEnv :: String -> Enviroment -> Maybe Type
isIdentInEnv name env = case match of
    Just (VarElem ident tp modality) -> Just tp
    Nothing -> case parentEnv of
        Just parent -> isIdentInEnv name parent
        Nothing -> Nothing
    where
        parentEnv = parent env
        varsEnv = vars env
        match = isIdentInVars name varsEnv

isIdentInVars :: String -> [EnviromentElement] -> Maybe EnviromentElement
isIdentInVars name [] = Nothing

isIdentInVars name ((VarElem ident tp modality):vars) = if (name == ident)
    then Just (VarElem ident tp modality)
    else isIdentInVars name vars

-- Restituisce il tipo di ritorno della funzione
getFunctionType :: FunCall -> Enviroment -> Err Type
getFunctionType (Call ident rExprsNode) env = isFunCallGood (getIdent ident) rExprsNode env

-- Controlla se la funzione è definita nell'ambiente (scope attuale o quello di un padre)
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

-- Controlla che i tipi dei parametri formali corrispondano a quelli dei tipi formali
isFunCallGood :: String -> [AbsNode] -> Enviroment -> Err Type
isFunCallGood funcName rExprsNode env = 
    case (isFuncInEnv funcName env) of
        Just (tp, params) ->
            case (get_RExprsNode rExprsNode params env) of
                Nothing -> Ok tp
                Just msg -> Bad ("Error in procedure call: " ++ funcName ++ " error: " ++ msg)
        Nothing -> Bad ("Function: " ++ funcName ++ " is not declared in the scope")

-- Utilizzata in caso di errore, per illustrare in maniera più semantica 
-- su che riga e collana è presente l'errore che lo ha generato
getNodeInfo :: AbsNode -> String
getNodeInfo node = let (Pn line column) = (pos node) in ("Error => (line: " ++ (show line) ++ " column: " ++ (show column) ++ ") ")

-- Inserisce nello scope più in alto le funzione prederifinite dal linguaggio
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


-- Le funzioni che iniziano con il nome "check_" lavorano sulla monade stato
-- Le funzioni che iniziano con il nome "get_" lavorano sul tipo Err Type, cioè restituiscono Err Type


check_Prog :: AbsNode -> State Attributes ()
check_Prog (ProgramNode posn (Prog decls)) = do
    addToEnvPrimitiveFunctions -- Aggiunge all'enviroment attuale (scope più in alto) le funzioni predefinite dal linguaggio
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
    -- Controlla che il tipo di ritorno di una funzione corrisponda al tipo dichiarato all'interno del costrutto "return"
    Dfun ident parametersNode basicTypeNode compStmtNode returnStmtNode -> do
        pushToEnv $ FuncElem (getIdent ident) (getType $ get_BasicTypeNode basicTypeNode) (serializeEnvParameters parametersNode)
        setNewEnv
        pushToEnvFuncParams parametersNode
        check_CompStmtNode compStmtNode
        env <- gets env
        case (let (ReturnStmtNode pos returnStmt) = returnStmtNode in (get_ReturnStmt returnStmt env)) of 
            Ok tp -> do
                case (checkTypes (get_BasicTypeNode basicTypeNode) (Ok tp)) of
                    Ok _ -> do
                        setOldEnv
                        return()
                    Bad msg -> do
                        setOldEnv
                        setError $ (getNodeInfo basicTypeNode) ++ "In function: " ++ (getIdent ident) ++ " declared type and returned type are not equal " ++ msg
                        return()
            Bad msg -> do
                setOldEnv
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
    setNewEnv
    check_DeclsNode decls
    check_StmtsNode stmts
    setOldEnv
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
            -- Controlla che in lExpr1 non ci siano errori di tipo
            case lExpr1 of
                -- Nel caso in cui lExpr è stato dichiarato con la modalità "val" (constant)
                -- in caso affermativo si genera un errore, una variabile in modalità read-only
                -- non può comparire a sinistra di un'assegnamento
                Ok tp -> if (isIdentVal_LExpr (gLExpr lExpr) env)
                    then
                        setError $ (getNodeInfo lExpr) ++ "identifier is defined as read only"
                    else
                        case rExpr1 of
                        -- Controlla che il tipo della parte sinistra sia compatibile con la parte destra
                        Ok tp -> case (checkTypes lExpr1 rExpr1) of
                            -- Check if it should be an aritmetic type
                            Ok tp -> case (assignment_op) of
                                (Assign) -> do return ()
                                -- Nel caso in cui abbiamo un assegnamento di tipo "a += b"
                                -- bisogna verificare se il tipo permette operazioni aritmetiche
                                -- in caso negativo viene generato un errore
                                (AssignOp _) -> case (checkAritmTypes lExpr1 rExpr1) of
                                    Ok tp -> do return ()
                                    Bad msg -> setError $ (getNodeInfo lExpr) ++ msg
                            Bad msg -> setError $ (getNodeInfo lExpr) ++ msg
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
        ExHandler tryCatchStmt -> do
            check_TryCatchStmtNode tryCatchStmt
            return ()

check_TryCatchStmtNode :: AbsNode -> State Attributes ()
check_TryCatchStmtNode (TryCatchStmtNode _ (TryCatch stmt1 ident stmt2)) = do
    check_StmtNode stmt1
    check_StmtNode stmt2
    return ()

check_JumpStmtNode :: AbsNode -> State Attributes ()
check_JumpStmtNode node = case node of
    (JumpStmtNode _ Break) -> do
        -- Il costrutto "break" può comparire soltanto in uno statement di iterazione
        -- in caso contrario viene generato un'errore
        isLoop <- gets isLoop
        if (isLoop)
        then do return ()
        else setError $ (getNodeInfo node) ++ "'break' statement must be used inside a loop block" 
        return ()
    (JumpStmtNode _ Continue) -> do
        -- Il costrutto "continue" può comparire soltanto in uno statement di iterazione
        -- in caso contrario viene generato un'errore
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
                -- Controlla se rExprType e di tipo booleano
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
                Bad msg -> setError $ (getNodeInfo rExpr1) ++ msg
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
            -- Controlla se rExprType e di tipo booleano
            Ok tp -> case (checkBoolType rExprType) of
                Ok tp -> check_StmtNode stmt
                Bad msg -> setError $ (getNodeInfo rExpr) ++ msg
            Bad msg -> setError $ (getNodeInfo rExpr) ++ msg
            where
                rExprType = get_RExprNode rExpr env
        IfElse rExpr stmt1 stmt2 -> case (rExprType) of
            -- Controlla se rExprType e di tipo booleano
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
    -- Controlla se la funzione chiamata è presente nello scope (cioè se esiste),
    -- controllo per vedere se i tipi dei parametri attuali corrispondono a quelli formali
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
    OpAritm rExpr1 rExpr2 _ -> case tp1 of
        Ok tp -> case tp2 of
            Ok tp -> checkAritmTypes tp1 tp2
            Bad msg -> Bad msg
        Bad msg -> Bad msg
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
    -- Controlla che il tipo sia di tipo TypePointer
    Deref rExpr -> case (get_RExprNode rExpr env) of
        Ok (TypePointer tp) -> Ok tp
        Ok _ -> Bad "expression is not a pointer"
        Bad msg -> Bad msg
    PreIncrDecr lExpr _ -> case tpLExpr of
        Ok tp -> case (checkAritmType tpLExpr) of
            Ok tp -> if (isIdentVal_LExpr (gLExpr lExpr) env)
                then Bad "identifier is defined as read only"
                else Ok tp
            Bad msg -> Bad msg
        Bad msg -> Bad msg
        where tpLExpr = get_LExprNode lExpr env
    PostIncrDecr lExpr _ -> case tpLExpr of
        Ok tp -> case (checkAritmType tpLExpr) of
            Ok tp -> if (isIdentVal_LExpr (gLExpr lExpr) env)
                then Bad "identifier is defined as read only"
                else Ok tp
            Bad msg -> Bad msg
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
        -- Controlla che la chiamata dell'array non vada fuori dalla sua dimensione dichiarata
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


-- Sotto seguono delle funzioni utilizzate per la
-- verifica di variabili di tipo "val" (constant)
-- verificano che non siano modificabili all'interno del codice sorgente
-- in caso negativo viene generato un'errore

isIdentVal_LExpr :: LExpr -> Enviroment -> Bool
isIdentVal_LExpr node env = case node of
    Deref (RExprNode _ rExpr) -> isIdentVal_RExpr rExpr env
    PreIncrDecr (LExprNode _ lExpr) _ -> isIdentVal_LExpr lExpr env
    PostIncrDecr (LExprNode _ lExpr) _ -> isIdentVal_LExpr lExpr env
    BasLExpr (BLExprNode _ bLExpr) -> isIdentVal_BLExpr bLExpr env

isIdentVal_RExpr :: RExpr -> Enviroment -> Bool
isIdentVal_RExpr node env = case node of
    OpAritm (RExprNode _ rExpr1) (RExprNode _ rExpr2) _ -> ((isIdentVal_RExpr rExpr1 env) || (isIdentVal_RExpr rExpr2 env))
    Not (RExprNode _ rExpr) -> isIdentVal_RExpr rExpr env
    Neg (RExprNode _ rExpr) -> isIdentVal_RExpr rExpr env
    Ref (LExprNode _ lExpr) -> isIdentVal_LExpr lExpr env
    Lexpr (LExprNode _ lExpr) -> isIdentVal_LExpr lExpr env
    otherwise -> False

isIdentVal_BLExpr :: BLExpr -> Enviroment -> Bool
isIdentVal_BLExpr node env = case node of
    ArrayEl (BLExprNode _ bLExpr) (RExprNode _ rExpr) -> ((isIdentVal_BLExpr bLExpr env) || (isIdentVal_RExpr rExpr env))
    Id ident -> isIdentVal (getIdent ident) env

------------------------------------------------------------
--------- Parser AbsNode -----------------------------------
------------------------------------------------------------


-- sotto seguono delle funzioni con l'unico scopo
-- di fare da proxy/mediatore, espandano un nodo chiamando la funzione
-- appropriata per il prossimo nodo che e stato smontato


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