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
                                (EnvTAC [] [] [] [] Nothing)            -- env
                                0                                       -- counterTemp
                                0                                       -- counterLab
                                ""                                      -- addr
                                ("","")                                 -- ttff
                                ""                                      -- next
                                ""                                      -- exit
                                False                                   -- isSelection
                                (ArrayAttr "" "" TypeUnit TypeUnit 0) -- array

------------------------------------------------------------
----------------------- ENVIRONMENT ------------------------
------------------------------------------------------------

data EnviromentTAC 
    = EnvTAC {
        vars :: [(String, String)],           -- couple (ident, label)
        arrays :: [ArrayElemTAC],
        pointers :: [(String, String)],
        funcs :: [(String, String)],
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

data ArrayElemTAC = ArrayElemTAC {ident :: String, label :: String, tpa :: Type}
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
    | TypeError String
    deriving (Eq, Show, Read)

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

pushPointerToEnv :: (String,String) -> State Attributes ()
pushPointerToEnv envElem = do
    currentEnv <- gets env
    modify (\attr -> attr {env = currentEnv {pointers = envElem : (pointers currentEnv)}})
    return ()

pushFuncToEnv :: (String,String) -> State Attributes ()
pushFuncToEnv envElem = do
    currentEnv <- gets env
    modify (\attr -> attr {env = currentEnv {funcs = envElem : (funcs currentEnv)}})
    return ()

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
    | TACAssignOp String String String
    | TACUnaryOp String String String
    | TACCondition String String String
    | TACIf TAC String String
    | TACGoto String
    | TACReturn String
    | TACPreamble String
    | TACParam String
    | TACCallVoid String String
    | TACCall String String String
    | TACLExpr String
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
    code_Decls decls
    addCode $ label ++ ":halt"
    addTAC $ TACLabel label
    addTAC $ TACPreamble "halt"
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
        addCode $ (getIdent ident) ++ "=" ++ addr_RExpr
        addTAC $ TACAssign (getIdent ident) addr_RExpr
        return ()
    DvarCInit _ ident tp complexRExpr ->
        case typeSpec of 
            TypeArray _ _ -> do
                temp <- newCounterTemp
                pushArrayToEnv $ ArrayElemTAC name temp typeSpec
                addCode $ temp ++ "=" ++ name
                addTAC $ TACAssign temp name
                code_DeclArray temp dimElems rExprs 0
                return ()
            TypePointer _ -> do
                return ()
        where
            name = (getIdent ident)
            typeSpec = (getTypeSpec tp)
            dimElems = (getDimType typeSpec)
            rExprs = (serializeCompRExprs complexRExpr)
    Dfun ident parameters _ compStmt returnStmt -> do
        label <- newCounterLab
        pushFuncToEnv ((getIdent ident),label)
        addCode $ label ++ ":"
        addCode "BeginFunc"
        addTAC $ TACLabel label
        addTAC $ TACPreamble "BeginFunc"
        code_CompStmt compStmt
        code_ReturnStmt returnStmt
        addCode "EndFunc"
        addTAC $ TACPreamble "EndFunc"
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
    code_Decls decls
    code_Stmts stmts
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
        addCode $ label
        addTAC $ TACLabel label
        return ()
    Sel selectionStmt -> do
        label <- newCounterLab
        modify (\attr -> attr{next = label})
        code_SelectionStmt selectionStmt
        addCode $ label
        addTAC $ TACLabel label
        return ()
    Assgn lExpr assignment_op rExpr -> do
        code_AssignmentOp lExpr assignment_op rExpr
        return ()
    LExprStmt lExpr -> do
        code_LExpr lExpr
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
        addTAC $ TACAssignOp addr_LExpr op addr_RExpr
        return ()

code_FunCall :: AbsNode -> State Attributes ()
code_FunCall (FunCallNode _ (Call ident rExprs)) = do
        code_CallParams rExprs []
        temp <- newCounterTemp
        modify (\attr -> attr{addr = temp})
        addrAttr <- gets addr
        addCode $ addrAttr ++ " = " ++ "Call " ++ (getIdent ident) ++ " " ++ (show $ length rExprs)
        addTAC $ TACCall addrAttr (getIdent ident) (show $ length rExprs)
        return () 

code_ProcCall :: AbsNode -> State Attributes ()
code_ProcCall (FunCallNode _ (Call ident rExprs)) = do
        code_CallParams rExprs []
        addCode $ "Call " ++ (getIdent ident) ++ " " ++ (show $ length rExprs)
        addTAC $ TACCallVoid (getIdent ident) (show $ length rExprs)
        return () 

code_CallParams :: [AbsNode] -> [String] -> State Attributes ()
code_CallParams (rExpr:rExprs) params = do
    code_RExpr rExpr
    addr_RExpr <- gets addr
    code_CallParams rExprs (addr_RExpr:params)
    return ()

code_CallParams [] params = do
    print_CallParams (reverse params)
    return ()

print_CallParams :: [String] -> State Attributes ()
print_CallParams (param:params) = do
    addCode $ "Param " ++ param
    addTAC $ TACParam param
    print_CallParams params
    return ()

print_CallParams [] = do
    return ()


code_IterStmt :: AbsNode -> State Attributes ()
code_IterStmt (IterStmtNode _ iterStmt) = case iterStmt of
    While rExpr stmt -> do
        nextL <- gets next
        beginL <- newCounterLab
        labelTT <- newCounterLab
        modify (\attr -> attr{ttff = (labelTT, nextL)})
        modify (\attr -> attr{exit = nextL})
        addCode $ beginL
        addTAC $ TACLabel beginL
        modify (\attr -> attr{isSelection = True})
        code_RExpr rExpr
        modify (\attr -> attr{isSelection = False})
        modify (\attr -> attr{next = beginL})
        addCode $ labelTT
        addTAC $ TACLabel labelTT
        code_Stmt stmt
        addCode $ "goto " ++ beginL
        addTAC $ TACGoto beginL
        return ()

code_SelectionStmt :: AbsNode -> State Attributes ()
code_SelectionStmt (SelectionStmtNode _ selectionStmt) = case selectionStmt of
    IfNoElse rExpr stmt -> do
        nextL <- gets next
        label <- newCounterLab
        modify (\attr -> attr{ttff = (label, nextL)})
        modify (\attr -> attr{isSelection = True})
        code_RExpr rExpr
        modify (\attr -> attr{isSelection = False})
        addCode $ label
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
        addCode labelTT
        addTAC $ TACLabel labelTT
        code_Stmt stmt1
        addCode $ " goto " ++ nextL
        addTAC $ TACGoto nextL
        addCode labelFF
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
        modify (\attr -> attr{isSelection = True})
        addTAC $ TACIf (TACCondition addr_RExpr1 op addr_RExpr2) tt ff
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
                    addCode label
                    addTAC $ TACLabel label
                    modify (\attr -> attr{ttff = (tt,ff)})
                    (code_RExpr rExpr2)
                    return ()
                "||" -> do
                    (tt,ff) <- gets ttff
                    label <- newCounterLab
                    modify (\attr -> attr{ttff = (tt,label)})
                    (code_RExpr rExpr1)
                    addCode label
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
        addTAC $ TACLExpr addr_LExpr
        return ()

code_LExpr :: AbsNode -> State Attributes ()
code_LExpr (LExprNode _ lExpr) = case lExpr of
    Deref rExpr -> do
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
                offsetT <- newCounterTemp
                addCode $ offsetT ++ " = " ++ offsetA ++ " + " ++ tmp
                modify (\attr -> attr{array = ArrayAttr offsetT baseA tpA tpElems dimElems})
                modify (\attr -> attr{addr = baseA ++ "[" ++ offsetT ++ "]"})
                return ()
                where
                    tpElems = getArraySubType tpElemA
                    dimElems = getArrayDimType tpElems
        return()
    Id ident -> do
        modify (\attr -> attr{addr = (getIdent ident)})
        return ()


