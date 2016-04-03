module CodeGenerator where

import Parser
import Control.Monad.State

tacGenerator abstractSyntaxTree = execState (code_Program abstractSyntaxTree) defaultAttributes

data Attributes = Attributes {
    code :: String,
    tac :: [TAC],
    env :: EnviromentTAC,
    counterTemp :: Int,
    counterLab :: Int,
    addr :: String,
    ttff :: (String,String),
    next :: String,
    exit :: String,
    isSelection :: Bool,
    array :: ArrayAttr
} deriving (Show)

defaultAttributes = Attributes  ""                                      -- code
                                []                                      -- tac
                                (EnvTAC [] [] [] Nothing)            -- env
                                0                                       -- counterTemp
                                0                                       -- counterLab
                                ""                                      -- addr
                                ("","")                                 -- ttff
                                ""                                      -- next
                                ""                                      -- exit
                                False                                   -- isSelection
                                (ArrayAttr "" "" TypeUnit TypeUnit 0)   -- array

predefinedFuncs = ["writeInt","writeFloat","writeChar","writeString",
                        "readInt","readFloat","readChar","readString"]

------------------------------------------------------------
----------------------- ENVIRONMENT ------------------------
------------------------------------------------------------

data EnviromentTAC 
    = EnvTAC {
        vars :: [(String, String)],           -- couple (ident, label)
        arrays :: [ArrayElemTAC],
        funcs :: [FuncElemTAC],
        parent :: Maybe EnviromentTAC
    }
    deriving (Show)

data ArrayAttr = ArrayAttr {
        offset :: String, 
        base :: String, 
        tp :: Type, 
        tpElem :: Type,
        tpElemWidth :: Int
    }
    deriving (Show, Eq)

data ArrayElemTAC = ArrayElemTAC {idA :: String, tempA :: String, tpA :: Type}
    deriving (Eq, Show)

data FuncElemTAC = FuncElemTAC {idF :: String, labF :: String, parF :: [ParamElemTAC]}
    deriving (Eq, Show)

data ParamElemTAC = ParamElemTAC {idP :: String, tempP :: String, modP :: ModalityParam}
    deriving (Eq, Show)

data Type 
    = TypeInt
    | TypeChar
    | TypeBool
    | TypeFloat
    | TypeString
    | TypeUnit
    | TypeArray Type (Maybe Int)
    | TypePointer Type
    deriving (Eq, Show, Read)

setNewEnv :: State Attributes ()
setNewEnv = do
    currentEnv <- gets env
    modify (\attr -> attr {env = (EnvTAC {vars = [], arrays = [], funcs = [], parent = Just currentEnv})})
    return ()

setOldEnv :: State Attributes ()
setOldEnv = do
    currentEnv <- gets env
    case (parent currentEnv) of
        Just parentEnv -> modify (\attr -> attr {env = parentEnv})
    return ()

pushVarToEnv :: (String,String) -> State Attributes ()
pushVarToEnv envElem = do
    currentEnv <- gets env
    modify (\attr -> attr {env = currentEnv {vars = envElem : (vars currentEnv)}})
    return ()

pushArrayToEnv :: ArrayElemTAC -> State Attributes ()
pushArrayToEnv envElem = do
    currentEnv <- gets env
    modify (\attr -> attr {env = currentEnv {arrays = envElem : (arrays currentEnv)}})
    return ()

pushFuncToEnv :: FuncElemTAC -> State Attributes ()
pushFuncToEnv envElem = do
    currentEnv <- gets env
    modify (\attr -> attr {env = currentEnv {funcs = envElem : (funcs currentEnv)}})
    return ()

isVarInEnv :: String -> EnviromentTAC -> Maybe (String,String)
isVarInEnv varName env = case match of
    Just varElem -> Just varElem
    Nothing -> case parentEnv of
        Just parent -> isVarInEnv varName parent
        Nothing -> Nothing
    where
        parentEnv = parent env
        match = isVarInVars varName (vars env)

isVarInVars :: String -> [(String,String)] -> Maybe (String,String)
isVarInVars varName [] = Nothing
isVarInVars varName ((ident,temp):vars) = if varName == ident
    then Just (ident,temp)
    else isVarInVars varName vars

isArrayInEnv :: String -> EnviromentTAC -> Maybe ArrayElemTAC
isArrayInEnv arrayName env = case match of
    Just arrayElem -> Just arrayElem
    Nothing -> case parentEnv of
        Just parent -> isArrayInEnv arrayName parent
        Nothing -> Nothing
    where
        parentEnv = parent env
        match = isArrayInArrays arrayName (arrays env)

isArrayInArrays :: String -> [ArrayElemTAC] -> Maybe ArrayElemTAC
isArrayInArrays arrayName [] = Nothing
isArrayInArrays arrayName ((ArrayElemTAC ident label tpa):arrays) = if arrayName == ident
    then Just (ArrayElemTAC ident label tpa)
    else isArrayInArrays arrayName arrays

isFuncInEnv :: String -> EnviromentTAC -> Maybe FuncElemTAC
isFuncInEnv funcName env = case match of
    Just funcElem -> Just funcElem
    Nothing -> case parentEnv of
        Just parent -> isFuncInEnv funcName parent
        Nothing -> Nothing
    where
        parentEnv = parent env
        match = isFuncInFuncs funcName (funcs env)

isFuncInFuncs :: String -> [FuncElemTAC] -> Maybe FuncElemTAC
isFuncInFuncs funcName [] = Nothing
isFuncInFuncs funcName ((FuncElemTAC ident label par):funcs) = if funcName == ident
    then Just (FuncElemTAC ident label par)
    else isFuncInFuncs funcName funcs

getTypeSpec :: AbsNode -> Type
getTypeSpec (TypeSpecNode _ typeSpec) = case typeSpec of
    BasTyp (BasicTypeNode _ (BType tp)) -> case tp of
        "Boolean"   -> TypeBool
        "Char"      -> TypeChar
        "Float"     -> TypeFloat
        "Int"       -> TypeInt
        "Unit"      -> TypeUnit
        "String"    -> TypeString
    CompType (CompoundTypeNode _ compType) -> case compType of
        ArrDef typeSpecNode (Just dim)  -> TypeArray (getTypeSpec typeSpecNode) (Just dim) 
        ArrDef typeSpecNode Nothing     -> TypeArray (getTypeSpec typeSpecNode) Nothing
        Pointer typeSpecNode            -> TypePointer (getTypeSpec typeSpecNode)

getDimType :: Type -> Int
getDimType tp = case tp of
    TypeBool    -> 1
    TypeChar    -> 16
    TypeFloat   -> 32
    TypeInt     -> 32 
    TypeUnit    -> 1
    TypeString  -> 32
    TypeArray tpa _ -> getDimType tpa
    TypePointer _ -> 32 

getArrayDimType :: Type -> Int
getArrayDimType tp = case tp of
    TypeBool    -> 1
    TypeChar    -> 16
    TypeFloat   -> 32
    TypeInt     -> 32 
    TypeUnit    -> 1
    TypeString  -> 32
    TypeArray tpa (Just dim) -> (getDimType tpa) * dim
    TypePointer _ -> 32

getArraySubType :: Type -> Type
getArraySubType (TypeArray tp _) = tp

------------------------------------------------------------
--------------------- FINE ENVIRONMENT ---------------------
------------------------------------------------------------


------------------------------------------------------------
--------------------------- TAC ----------------------------
------------------------------------------------------------
data TAC
    = TACLabel String
    | TACBinaryOp String String String String
    | TACAssign String String
    | TACUnaryOp String String String
    | TACCondition String String String
    | TACIf TAC String String
    | TACGoto String
    | TACReturn String
    | TACPreamble String
    | TACParam String
    | TACCallVoid String String
    | TACCall String String String
    | TACException String
    deriving (Show)

addTAC :: TAC -> State Attributes ()
addTAC tacData = do
    modify (\attr -> attr{tac = (tac attr) ++ [tacData]})
    return ()

------------------------------------------------------------
------------------------ Utilities -------------------------
------------------------------------------------------------

increaseCounterTemp :: Attributes -> Attributes
increaseCounterTemp attr = attr {counterTemp = (counterTemp attr) + 1}

increaseCounterLab :: Attributes -> Attributes
increaseCounterLab attr = attr {counterLab = (counterLab attr) + 1}

getIdent :: Ident -> String
getIdent (Ident ident) = ident

serializeCompRExprs :: AbsNode -> [AbsNode]
serializeCompRExprs (ComplexRExprNode _ complexRExpr) = case complexRExpr of
    Simple rExprNode -> [rExprNode]
    Array complexRExprs -> foldr (++) [] (map (serializeCompRExprs) complexRExprs) 

serializeParams :: [AbsNode] -> State Attributes ([ParamElemTAC])
serializeParams (param:params) = do
    temp <- newCounterTemp
    listParams <- serializeParams params
    return ((ParamElemTAC ident temp modality):listParams)
    where
        ParameterNode _ (Param modalityNode (Ident ident) _) = param
        ModalityParamNode _ modality = modalityNode

serializeParams [] = do
    return ([]) 

addCode :: String -> State Attributes ()
addCode newCode = modify (\attr -> attr{code = (code attr) ++ newCode ++ "\n"})

newCounterLab :: State Attributes (String)
newCounterLab = do
    modify increaseCounterLab
    label <- gets counterLab
    return ("L" ++ (show label))

newCounterTemp :: State Attributes (String)
newCounterTemp = do
    modify increaseCounterTemp
    temp <- gets counterTemp
    return ("T" ++ (show temp))

------------------------------------------------------------
---------------------- Code Generator ----------------------
------------------------------------------------------------

code_Program :: AbsNode -> State Attributes ()
code_Program (ProgramNode _ (Prog decls)) = do
    label <- newCounterLab
    code_PredefinedFuncs predefinedFuncs
    code_Decls decls
    addCode $ label ++ ":halt"
    addTAC $ TACLabel label
    addTAC $ TACPreamble "halt"
    return ()

code_PredefinedFuncs :: [String] -> State Attributes ()
code_PredefinedFuncs (func:funcs) = do
    label <- newCounterLab
    pushFuncToEnv $ FuncElemTAC func label [(ParamElemTAC "" "" ModalityP_val)]
    addCode $ label ++ ":"
    addTAC $ TACLabel label
    addCode "BeginFunc"
    addTAC $ TACPreamble "BeginFunc"
    addCode "EndFunc"
    addTAC $ TACPreamble "EndFunc"
    code_PredefinedFuncs funcs
    return ()

code_PredefinedFuncs [] = do
    return ()

code_Decls :: [AbsNode] -> State Attributes ()
code_Decls (x:xs) = do
    code_Decl x
    code_Decls xs
    return ()

code_Decls [] = do
    return ()

code_Decl :: AbsNode -> State Attributes ()
code_Decl (DeclNode _ decl) = case decl of
    DvarBInit _ ident _ (ComplexRExprNode _ (Simple rExpr)) -> do
        (code_RExpr rExpr)
        addr_RExpr <- gets addr
        tmp <- newCounterTemp
        pushVarToEnv (name,tmp)
        addCode $ name ++ "=" ++ addr_RExpr
        addTAC $ TACAssign name addr_RExpr
        addCode $ tmp ++ "=" ++ name
        addTAC $ TACAssign tmp name
        return ()
        where 
            name = getIdent ident 
    DvarCInit _ ident tp complexRExpr ->
        case typeSpec of 
            TypeArray _ _ -> do
                temp <- newCounterTemp
                pushArrayToEnv $ ArrayElemTAC name temp typeSpec
                addCode $ temp ++ "=" ++ name
                addTAC $ TACAssign temp name
                code_DeclArray temp dimElems rExprs 0
                return ()
            TypePointer _ -> 
                case complexRExpr of
                    ComplexRExprNode _ (Simple rExpr) -> do
                        (code_RExpr rExpr)
                        addr_RExpr <- gets addr
                        tmp <- newCounterTemp
                        pushVarToEnv (name,tmp)
                        case head(addr_RExpr) of
                            '&' -> do
                                addCode $ name ++ "=" ++ addr_RExpr
                                addTAC $ TACAssign name addr_RExpr
                                return ()
                            _   -> do
                                addCode $ "*" ++ name ++ "=" ++ addr_RExpr
                                addTAC $ TACAssign ("*" ++ name) addr_RExpr
                                return ()
                        addCode $ tmp ++ " = " ++ name
                        addTAC $ TACAssign tmp name
                        return ()
        where
            name = (getIdent ident)
            typeSpec = (getTypeSpec tp)
            dimElems = (getDimType typeSpec)
            rExprs = (serializeCompRExprs complexRExpr)
    Dfun ident parameters _ compStmt returnStmt -> do
        label <- newCounterLab
        params <- serializeParams parameters
        pushFuncToEnv $ FuncElemTAC (getIdent ident) label params
        setNewEnv
        pushFuncParamsToEnv params
        addCode $ label ++ ":"
        addTAC $ TACLabel label
        addCode "BeginFunc"
        addTAC $ TACPreamble "BeginFunc"
        code_CompStmt compStmt
        code_ReturnStmt returnStmt
        addCode "EndFunc"
        addTAC $ TACPreamble "EndFunc"
        setOldEnv
        return ()

code_DeclArray :: String -> Int -> [AbsNode] -> Int -> State Attributes ()
code_DeclArray base dimElems (rExpr:rExprs) offset = do
    (code_RExpr rExpr)
    addr_RExpr <- gets addr
    addCode $ base ++ "[" ++ (show offset) ++ "] = " ++ addr_RExpr
    addTAC $ TACAssign (base ++ "[" ++ (show offset) ++ "]") addr_RExpr
    code_DeclArray base dimElems rExprs (offset + dimElems)
    return ()  
code_DeclArray base dimElems [] offset = do
    return ()  

code_ReturnStmt :: AbsNode -> State Attributes ()
code_ReturnStmt (ReturnStmtNode _ returnStmt) = case returnStmt of
    RetExpVoid -> do
        addCode "Return"
        addTAC $ TACReturn ""
        return ()
    RetExp rExpr -> do
        code_RExpr rExpr
        addr_RExpr <- gets addr
        addCode $ "Return " ++ addr_RExpr
        addTAC $ TACReturn addr_RExpr
        return () 

code_CompStmt :: AbsNode -> State Attributes ()
code_CompStmt (CompStmtNode _ (BlockDecl decls stmts)) = do
    setNewEnv
    code_Decls decls
    code_Stmts stmts
    setOldEnv
    return ()

code_Stmts :: [AbsNode] -> State Attributes ()
code_Stmts (stmt:stmts) = do
    code_Stmt stmt
    code_Stmts stmts
    return ()

code_Stmts [] = do
    return ()

code_Stmt :: AbsNode -> State Attributes ()
code_Stmt (StmtNode _ stmt) = case stmt of
    Comp compStmt -> do
        code_CompStmt compStmt
        return ()
    ProcCall funCall -> do
        code_ProcCall funCall
        return ()
    Jmp jumpStmt -> do
        code_JumpStmt jumpStmt
        return ()
    Iter iterStmt -> do
        label <- newCounterLab
        modify (\attr -> attr{next = label})
        code_IterStmt iterStmt
        addCode $ label ++ ":"
        addTAC $ TACLabel label
        return ()
    Sel selectionStmt -> do
        label <- newCounterLab
        modify (\attr -> attr{next = label})
        code_SelectionStmt selectionStmt
        addCode $ label ++ ":"
        addTAC $ TACLabel label
        return ()
    Assgn lExpr assignment_op rExpr -> do
        code_AssignmentOp lExpr assignment_op rExpr
        return ()
    LExprStmt lExpr -> do
        code_LExpr lExpr
        return ()
    ExHandler tryCatch -> do
        code_TryCatch tryCatch
        return () 

code_TryCatch :: AbsNode -> State Attributes ()
code_TryCatch (TryCatchStmtNode _ tryCatch) = case tryCatch of
    TryCatch stmtTry ident stmtCatch -> do
        nextL <- gets next
        beginCatch <- newCounterLab
        beginTry <- newCounterLab
        addCode $ "goto " ++ beginTry 
        addTAC $ TACGoto beginTry
        addCode $ beginCatch ++ ":"
        addTAC $ TACLabel beginCatch
        (code_Stmt stmtCatch)
        addCode $ "goto " ++ nextL
        addTAC $ TACGoto nextL
        addCode $ beginTry ++ ":"
        addTAC $ TACLabel beginTry
        addCode $ "on exception goto " ++ beginCatch
        addTAC $ TACException beginCatch
        (code_Stmt stmtTry)
        addCode $ nextL ++ ":"
        addTAC $ TACLabel nextL
        return () 

code_JumpStmt :: AbsNode -> State Attributes ()
code_JumpStmt (JumpStmtNode _ jumpStmt) = case jumpStmt of
    Break       -> do
        exitL <- gets exit
        addCode $ "goto " ++ exitL
        addTAC $ TACGoto exitL
        return ()
    Continue    -> do
        nextL <- gets next
        addCode $ "goto " ++ nextL
        addTAC $ TACGoto nextL
        return ()

code_AssignmentOp :: AbsNode -> AbsNode -> AbsNode -> State Attributes ()
code_AssignmentOp lExpr (Assignment_opNode _ assignment_op) rExpr = case assignment_op of
    Assign      -> do
        (code_LExpr lExpr)
        addr_LExpr <- gets addr
        (code_RExpr rExpr)
        addr_RExpr <- gets addr
        addCode $ addr_LExpr ++ "=" ++ addr_RExpr
        addTAC $ TACAssign addr_LExpr addr_RExpr
        return ()
    AssignOp op  -> do
        (code_LExpr lExpr)
        addr_LExpr <- gets addr
        (code_RExpr rExpr)
        addr_RExpr <- gets addr
        addCode $ addr_LExpr ++ "=" ++ addr_LExpr ++ op ++ addr_RExpr
        addTAC $ TACBinaryOp addr_LExpr addr_LExpr op addr_RExpr
        return ()

code_FunCall :: AbsNode -> State Attributes ()
code_FunCall (FunCallNode _ (Call ident rExprs)) = do
        params <- code_CallParams rExprs
        print_CallParams params
        env <- gets env
        case (isFuncInEnv (getIdent ident) env) of
            Just (FuncElemTAC _ label paramsIn) -> do
                temp <- newCounterTemp
                modify (\attr -> attr{addr = temp})
                addrAttr <- gets addr
                addCode $ addrAttr ++ " = " ++ "Call " ++ label ++ " " ++ (show $ length rExprs)
                addTAC $ TACCall addrAttr label (show $ length rExprs)
                code_ModalityParams params paramsIn
                return ()
        return () 

code_ProcCall :: AbsNode -> State Attributes ()
code_ProcCall (FunCallNode _ (Call ident rExprs)) = do
        params <- code_CallParams rExprs
        print_CallParams params
        env <- gets env
        case (isFuncInEnv (getIdent ident) env) of
            Just (FuncElemTAC _ label paramsIn) -> do
            addCode $ "Call " ++ label ++ " " ++ (show $ length rExprs)
            addTAC $ TACCallVoid label (show $ length rExprs)
            code_ModalityParams params paramsIn
            return ()
        return () 

code_ModalityParams :: [String] -> [ParamElemTAC] -> State Attributes ()
code_ModalityParams (param:params) ((ParamElemTAC idP tempP ModalityP_valres):paramsIn) = do
    addCode $ param ++ " = " ++ tempP
    addTAC $ TACAssign param tempP
    code_ModalityParams params paramsIn
    return () 

code_ModalityParams (_:params) ((ParamElemTAC _ _ _):paramsIn) = do
    code_ModalityParams params paramsIn
    return () 

code_ModalityParams [] [] = do 
    return ()

code_CallParams :: [AbsNode] -> State Attributes ([String])
code_CallParams (rExpr:rExprs) = do
    code_RExpr rExpr
    addr_RExpr <- gets addr
    addrs_RExprs <- code_CallParams rExprs
    return (addr_RExpr:addrs_RExprs)

code_CallParams [] = do
    return ([])

print_CallParams :: [String] -> State Attributes ()
print_CallParams (param:params) = do
    addCode $ "Param " ++ param
    addTAC $ TACParam param
    print_CallParams params
    return ()

print_CallParams [] = do
    return ()

pushFuncParamsToEnv :: [ParamElemTAC] -> State Attributes ()
pushFuncParamsToEnv ((ParamElemTAC ident temp modality):params) = do
    pushVarToEnv (ident,temp) 
    addCode $ temp ++ " = " ++ ident
    addTAC $ TACAssign temp ident
    pushFuncParamsToEnv params
    return ()

pushFuncParamsToEnv [] = do 
    return ()

code_IterStmt :: AbsNode -> State Attributes ()
code_IterStmt (IterStmtNode _ iterStmt) = case iterStmt of
    While rExpr stmt -> do
        nextL <- gets next
        beginL <- newCounterLab
        labelTT <- newCounterLab
        modify (\attr -> attr{ttff = (labelTT, nextL)})
        modify (\attr -> attr{exit = nextL})
        addCode $ beginL ++ ":"
        addTAC $ TACLabel beginL
        modify (\attr -> attr{isSelection = True})
        code_RExpr rExpr
        modify (\attr -> attr{isSelection = False})
        modify (\attr -> attr{next = beginL})
        addCode $ labelTT ++ ":"
        addTAC $ TACLabel labelTT
        code_Stmt stmt
        addCode $ "goto " ++ beginL
        addTAC $ TACGoto beginL
        return ()
    For ident start end stmt -> do
        nextL <- gets next
        (code_RExpr start)
        addr_Start <- gets addr
        (code_RExpr end)
        addr_End <- gets addr
        beginL <- newCounterLab
        labelTT <- newCounterLab
        modify (\attr -> attr{exit = nextL})
        ic <- newCounterTemp
        temp <- newCounterTemp
        addCode $ temp ++ " = " ++ addr_End ++ " - " ++ addr_Start
        addTAC $ TACBinaryOp temp addr_End "-" addr_Start
        addCode $ ic ++ " = " ++ temp ++ " + 1"
        addTAC $ TACBinaryOp ic temp "+" "1"
        addCode $ var ++ " = " ++ ic 
        addTAC $ TACAssign var ic
        addCode $ beginL ++ ":"
        addTAC $ TACLabel beginL 
        addCode $ "if " ++ var ++ "<=" ++ addr_End ++ " goto " ++ labelTT
        addCode $ "goto " ++ nextL
        addTAC $ TACIf (TACCondition var "<=" addr_End) labelTT nextL
        addCode $ labelTT ++ ": "
        addTAC $ TACLabel labelTT
        (code_Stmt stmt)
        addCode $ ic ++ " = " ++ ic ++ " + 1"
        addTAC $ TACBinaryOp ic ic "+" "1"
        addCode $ var ++ " = " ++ ic 
        addTAC $ TACAssign var ic
        addCode $ "goto " ++ beginL
        addTAC $ TACGoto beginL 
        return ()
        where 
            var = getIdent ident

code_SelectionStmt :: AbsNode -> State Attributes ()
code_SelectionStmt (SelectionStmtNode _ selectionStmt) = case selectionStmt of
    IfNoElse rExpr stmt -> do
        nextL <- gets next
        label <- newCounterLab
        modify (\attr -> attr{ttff = (label, nextL)})
        modify (\attr -> attr{isSelection = True})
        code_RExpr rExpr
        modify (\attr -> attr{isSelection = False})
        addCode $ label ++ ":"
        addTAC $ TACLabel label
        code_Stmt stmt
        return ()
    IfElse rExpr stmt1 stmt2 -> do
        nextL <- gets next
        labelTT <- newCounterLab
        labelFF <- newCounterLab
        modify (\attr -> attr{ttff = (labelTT, labelFF)})
        modify (\attr -> attr{isSelection = True})
        code_RExpr rExpr
        modify (\attr -> attr{isSelection = False})
        addCode $ labelTT ++ ":"
        addTAC $ TACLabel labelTT
        code_Stmt stmt1
        addCode $ " goto " ++ nextL
        addTAC $ TACGoto nextL
        addCode $ labelFF ++ ":"
        addTAC $ TACLabel labelFF
        code_Stmt stmt2
        return ()

code_RExpr :: AbsNode -> State Attributes ()
code_RExpr (RExprNode _ rExpr) = case rExpr of
    OpRelation rExpr1 rExpr2 op -> do
        modify (\attr -> attr{isSelection = False})
        (tt,ff) <- gets ttff
        (code_RExpr rExpr1)
        addr_RExpr1 <- gets addr
        (code_RExpr rExpr2)
        addr_RExpr2 <- gets addr
        addCode $ "if " ++ addr_RExpr1 ++ op ++ addr_RExpr2 ++ " goto " ++ tt ++ " goto " ++ ff
        addTAC $ TACIf (TACCondition addr_RExpr1 op addr_RExpr2) tt ff
        modify (\attr -> attr{isSelection = True})
        return ()
    OpAritm rExpr1 rExpr2 op -> do
        (code_RExpr rExpr1)
        addr_RExpr1 <- gets addr
        (code_RExpr rExpr2)
        addr_RExpr2 <- gets addr
        temp <- newCounterTemp
        modify (\attr -> attr{addr = temp})
        addrAttr <- gets addr
        addCode $ addrAttr ++ "=" ++ addr_RExpr1 ++ op ++ addr_RExpr2
        addTAC $ TACBinaryOp addrAttr addr_RExpr1 op addr_RExpr2
        return ()
    OpBoolean rExpr1 rExpr2 op -> do
        isSel <- gets isSelection
        if isSel
            then 
                case op of 
                "&&" -> do
                    (tt,ff) <- gets ttff
                    label <- newCounterLab
                    modify (\attr -> attr{ttff = (label,ff)})
                    (code_RExpr rExpr1)
                    addCode $ label ++ ":"
                    addTAC $ TACLabel label
                    modify (\attr -> attr{ttff = (tt,ff)})
                    (code_RExpr rExpr2)
                    return ()
                "||" -> do
                    (tt,ff) <- gets ttff
                    label <- newCounterLab
                    modify (\attr -> attr{ttff = (tt,label)})
                    (code_RExpr rExpr1)
                    addCode $ label ++ ":"
                    addTAC $ TACLabel label
                    modify (\attr -> attr{ttff = (tt,ff)})
                    (code_RExpr rExpr2)
                    return ()
            else    
                do
                (code_RExpr rExpr1)
                addr_RExpr1 <- gets addr
                (code_RExpr rExpr2)
                addr_RExpr2 <- gets addr
                temp <- newCounterTemp
                modify (\attr -> attr{addr = temp})
                addrAttr <- gets addr
                addCode $ addrAttr ++ "=" ++ addr_RExpr1 ++ op ++ addr_RExpr2
                addTAC $ TACBinaryOp addrAttr addr_RExpr1 op addr_RExpr2
                return ()
    Not rExpr -> do
        (tt,ff) <- gets ttff
        modify (\attr -> attr{ttff = (ff,tt)})
        code_RExpr rExpr
        return ()
    Neg rExpr -> do
        (code_RExpr rExpr)
        addr_RExpr <- gets addr
        temp <- newCounterTemp
        modify (\attr -> attr{addr = temp})
        addrAttr <- gets addr
        addCode $ addrAttr ++ " = negate" ++ addr_RExpr
        addTAC $ TACUnaryOp addrAttr "-" addr_RExpr
        return ()
    Ref lExpr -> do
        (code_LExpr lExpr)
        modify (\attr -> attr{addr = "&" ++ (addr attr)})
        return ()
    FCall funCall -> do
        code_FunCall funCall
        return ()
    Int int -> do
        modify (\attr -> attr{addr = (show int)})
        return ()
    Char char -> do
        modify (\attr -> attr{addr = ("\'" ++ (char:[]) ++ "\'")})
        return ()
    String string -> do
        modify (\attr -> attr{addr = ("\"" ++ string ++ "\"")})
        return ()
    Float float -> do
        modify (\attr -> attr{addr = (show float)})
        return ()
    Bool boolean -> 
        case boolean of
            Boolean_True -> do
                isSelection <- gets isSelection
                if isSelection
                    then
                        do
                        (tt,_) <- gets ttff 
                        addCode $ " goto " ++ tt
                        addTAC $ TACGoto tt
                        return ()
                    else
                        do
                        modify (\attr -> attr{addr = "True"})
                        return ()
            Boolean_False -> do
                isSelection <- gets isSelection
                if isSelection
                    then
                        do
                        (_,ff) <- gets ttff 
                        addCode $ " goto " ++ ff
                        addTAC $ TACGoto ff
                        return ()
                    else
                        do
                        modify (\attr -> attr{addr = "False"})
                        return ()
    Lexpr lExpr -> do
        code_LExpr lExpr
        addr_LExpr <- gets addr
        -- TODO: serve addTAC qua??
        --addTAC $ TACLExpr addr_LExpr
        return ()

code_LExpr :: AbsNode -> State Attributes ()
code_LExpr (LExprNode _ lExpr) = case lExpr of
    Deref rExpr -> do
        (code_RExpr rExpr)
        modify (\attr -> attr{addr = "*" ++ (addr attr)})
        return ()
    PreIncrDecr lExpr op -> do
        code_LExpr lExpr
        addr_LExpr <- gets addr
        addCode $ addr_LExpr ++ " = " ++ addr_LExpr ++ " " ++ op ++ " 1"
        addTAC $ TACBinaryOp addr_LExpr addr_LExpr op "1"
        temp <- newCounterTemp
        modify (\attr -> attr{addr = temp})
        addr_Attr <- gets addr
        addCode $ addr_Attr ++ " = " ++ addr_LExpr
        addTAC $ TACAssign addr_Attr addr_LExpr
        return ()
    PostIncrDecr lExpr op -> do
        -- TODO sistemare per l-value
        code_LExpr lExpr
        addr_LExpr <- gets addr
        temp <- newCounterTemp
        modify (\attr -> attr{addr = temp})
        addr_Attr <- gets addr
        addCode $ addr_Attr ++ " = " ++ addr_LExpr
        addTAC $ TACAssign addr_Attr addr_LExpr
        addCode $ addr_LExpr ++ " = " ++ addr_LExpr ++ " " ++ op ++ " 1"
        addTAC $ TACBinaryOp addr_LExpr addr_LExpr op "1"
        return ()
    BasLExpr bLExpr -> do
        code_BLExpr bLExpr
        return () 

-- Gli array partono da 0
code_BLExpr :: AbsNode -> State Attributes ()
code_BLExpr (BLExprNode _ bLExpr) = case bLExpr of
    ArrayEl (BLExprNode _ (Id ident)) rExpr -> do 
        env_Attr <- gets env
        case (isArrayInEnv name env_Attr) of
            Just (ArrayElemTAC _ label tp) -> do
                (code_RExpr rExpr)
                addr_RExpr <- gets addr
                offsetT <- newCounterTemp
                addCode $ offsetT ++ " = " ++ addr_RExpr ++ " * " ++ (show dimElems)
                addTAC $ TACBinaryOp offsetT addr_RExpr "*" (show dimElems)
                modify (\attr -> attr{array = ArrayAttr offsetT name tp tpElems dimElems})
                modify (\attr -> attr{addr = name ++ "[" ++ offsetT ++ "]"})
                return ()
                where 
                    tpElems = getArraySubType tp
                    dimElems = getArrayDimType tpElems
        return ()
        where
            name = getIdent ident

    ArrayEl bLExpr rExpr -> do 
        (code_RExpr rExpr)
        addr_RExpr <- gets addr
        (code_BLExpr bLExpr)
        array_BLExpr <- gets array
        case array_BLExpr of
            ArrayAttr offsetA baseA tpA tpElemA tpElemWidthA -> do
                tmp <- newCounterTemp
                addCode $ tmp ++ " = " ++ addr_RExpr ++ " * " ++ (show dimElems)
                addTAC $ TACBinaryOp tmp addr_RExpr "*" (show dimElems)
                offsetT <- newCounterTemp
                addCode $ offsetT ++ " = " ++ offsetA ++ " + " ++ tmp
                addTAC $ TACBinaryOp offsetT offsetA "+" tmp
                modify (\attr -> attr{array = ArrayAttr offsetT baseA tpA tpElems dimElems})
                modify (\attr -> attr{addr = baseA ++ "[" ++ offsetT ++ "]"})
                return ()
                where
                    tpElems = getArraySubType tpElemA
                    dimElems = getArrayDimType tpElems
        return()
    Id ident -> do
        env_Attr <- gets env
        case (isVarInEnv (getIdent ident) env_Attr) of
            Just (_,temp) -> do
                modify (\attr -> attr{addr = temp})
                return ()
        return ()


