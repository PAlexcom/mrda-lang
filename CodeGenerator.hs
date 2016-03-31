module CodeGenerator where

import Parser
import Control.Monad.State

tacGenerator abstractSyntaxTree = execState (code_Program abstractSyntaxTree) defaultAttributes

data Attributes = Attributes {
    code :: String,
    tac :: [TAC],
    env :: [Char],
    counterTemp :: Int,
    counterLab :: Int,
    addr :: String,
    ttff :: (String,String),
    next :: String,
    exit :: String,
    isSelection :: Bool
} deriving (Show)

defaultAttributes = Attributes "" [] [] 0 0 "" ("","") "" "" False


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

------------------------------------------------------------
---------------------- Code Generator ----------------------
------------------------------------------------------------

code_Program :: AbsNode -> State Attributes ()
code_Program (ProgramNode _ (Prog decls)) = do
    modify increaseCounterLab
    label <- gets counterLab
    code_Decls decls
    modify (\attr -> attr{code = (code attr) ++ "L" ++ (show label) ++ ":halt" ++ "\n"})
    addTAC $ TACLabel ("L" ++ (show label))
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
    DvarBInit _ ident _ complexRExpr -> do
        (code_ComplexRExpr complexRExpr)
        expr_attr <- get
        modify (\attr -> attr{code = (code attr) ++ (getIdent ident) ++ "=" ++ (addr expr_attr) ++ "\n"})
        addTAC $ TACAssign (getIdent ident) (addr expr_attr)
        return ()
    DvarCInit _ ident _ complexRExpr -> do
        (code_ComplexRExpr complexRExpr)
        expr_attr <- get
        modify (\attr -> attr{code = (code attr) ++ (getIdent ident) ++ "=" ++ (addr expr_attr) ++ "\n"})
        addTAC $ TACAssign (getIdent ident) (addr expr_attr)
        return ()
    Dfun ident parameters _ compStmt returnStmt -> do
        modify (\attr -> attr{code = (code attr) ++ (getIdent ident) ++ ":\n" ++ "BeginFunc" ++ "\n"})
        addTAC $ TACLabel (getIdent ident)
        addTAC $ TACPreamble "BeginFunc"
        code_CompStmt compStmt
        code_ReturnStmt returnStmt
        modify (\attr -> attr{code = (code attr) ++ "EndFunc" ++ "\n"})
        addTAC $ TACPreamble "EndFunc"
        return ()

code_ReturnStmt :: AbsNode -> State Attributes ()
code_ReturnStmt (ReturnStmtNode _ returnStmt) = case returnStmt of
    RetExpVoid -> do
        modify (\attr -> attr{code = (code attr) ++ "Return" ++ "\n"})
        addTAC $ TACReturn ""
        return ()
    RetExp rExpr -> do
        code_RExpr rExpr
        addr_RExpr <- gets addr
        modify (\attr -> attr{code = (code attr) ++ "Return " ++ addr_RExpr ++ "\n"})
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
        modify increaseCounterLab
        label <- gets counterLab
        modify (\attr -> attr{next = ("L" ++ (show label))})
        code_IterStmt iterStmt
        modify (\attr -> attr{code = (code attr) ++ "L" ++ (show label) ++ ":\n"})
        addTAC $ TACLabel ("L" ++ (show label))
        return ()
    Sel selectionStmt -> do
        modify increaseCounterLab
        label <- gets counterLab
        modify (\attr -> attr{next = ("L" ++ (show label))})
        code_SelectionStmt selectionStmt
        modify (\attr -> attr{code = (code attr) ++ "L" ++ (show label) ++ ":\n"})
        addTAC $ TACLabel ("L" ++ (show label))
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
        modify (\attr -> attr{code = (code attr) ++ "goto " ++ exitL ++ "\n"})
        addTAC $ TACGoto exitL
        return ()
    Continue    -> do
        nextL <- gets next
        modify (\attr -> attr{code = (code attr) ++ "goto " ++ nextL ++ "\n"})
        addTAC $ TACGoto nextL
        return ()

code_AssignmentOp :: AbsNode -> AbsNode -> AbsNode -> State Attributes ()
code_AssignmentOp lExpr (Assignment_opNode _ assignment_op) rExpr = case assignment_op of
    Assign      -> do
        (code_LExpr lExpr)
        addr_LExpr <- gets addr
        (code_RExpr rExpr)
        addr_RExpr <- gets addr
        modify (\attr -> attr{code = (code attr) ++ addr_LExpr ++ "=" ++ addr_RExpr ++ "\n"})
        addTAC $ TACAssign addr_LExpr addr_RExpr
        return ()
    AssignOp op  -> do
        (code_LExpr lExpr)
        addr_LExpr <- gets addr
        (code_RExpr rExpr)
        addr_RExpr <- gets addr
        modify (\attr -> attr{code = (code attr) ++ addr_LExpr ++ "=" ++ addr_LExpr ++ op ++ addr_RExpr ++ "\n"})
        addTAC $ TACAssignOp addr_LExpr op addr_RExpr
        return ()

code_FunCall :: AbsNode -> State Attributes ()
code_FunCall (FunCallNode _ (Call ident rExprs)) = do
        code_CallParams rExprs []
        modify increaseCounterTemp
        modify (\attr -> attr{addr = "t" ++ (show $ counterTemp attr)})
        modify (\attr -> attr{code = (code attr) ++ (addr attr) ++ "= " ++ "Call " ++ (getIdent ident) ++ " " ++ (show $ length rExprs) ++ "\n"})
        addrAttr <- gets addr
        addTAC $ TACCall addrAttr (getIdent ident) (show $ length rExprs)
        return () 

code_ProcCall :: AbsNode -> State Attributes ()
code_ProcCall (FunCallNode _ (Call ident rExprs)) = do
        code_CallParams rExprs []
        modify (\attr -> attr{code = (code attr) ++ "Call " ++ (getIdent ident) ++ " " ++ (show $ length rExprs) ++ "\n"})
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
    modify (\attr -> attr{code = (code attr) ++ "Param " ++ param ++ "\n"})
    addTAC $ TACParam param
    print_CallParams params
    return ()

print_CallParams [] = do
    return ()


code_IterStmt :: AbsNode -> State Attributes ()
code_IterStmt (IterStmtNode _ iterStmt) = case iterStmt of
    While rExpr stmt -> do
        nextL <- gets next
        modify increaseCounterLab
        beginL <- gets counterLab
        modify increaseCounterLab
        labelTT <- gets counterLab
        modify (\attr -> attr{ttff = ("L" ++ (show labelTT), nextL)})
        modify (\attr -> attr{exit = nextL})
        modify (\attr -> attr{code = (code attr) ++ "L" ++ (show beginL) ++ ":\n"})
        addTAC $ TACLabel ("L" ++ (show beginL))
        modify (\attr -> attr{isSelection = True})
        code_RExpr rExpr
        modify (\attr -> attr{isSelection = False})
        modify (\attr -> attr{next = "L" ++ (show beginL)})
        modify (\attr -> attr{code = (code attr) ++ "L" ++ (show labelTT) ++ ":\n"})
        addTAC $ TACLabel ("L" ++ (show labelTT))
        code_Stmt stmt
        modify (\attr -> attr{code = (code attr) ++ "goto L" ++ (show beginL) ++ "\n"})
        addTAC $ TACGoto $ "L" ++ (show beginL)
        return ()

code_SelectionStmt :: AbsNode -> State Attributes ()
code_SelectionStmt (SelectionStmtNode _ selectionStmt) = case selectionStmt of
    IfNoElse rExpr stmt -> do
        nextL <- gets next
        modify increaseCounterLab
        label <- gets counterLab
        modify (\attr -> attr{ttff = ("L" ++ (show label), nextL)})
        modify (\attr -> attr{isSelection = True})
        code_RExpr rExpr
        modify (\attr -> attr{isSelection = False})
        modify (\attr -> attr{code = (code attr) ++ "L" ++ (show label) ++ ":\n"})
        addTAC $ TACLabel $ "L" ++ (show label)
        code_Stmt stmt
        return ()
    IfElse rExpr stmt1 stmt2 -> do
        nextL <- gets next
        modify increaseCounterLab
        labelTT <- gets counterLab
        modify increaseCounterLab
        labelFF <- gets counterLab
        modify (\attr -> attr{ttff = ("L" ++ (show labelTT), "L" ++ (show labelFF))})
        modify (\attr -> attr{isSelection = True})
        code_RExpr rExpr
        modify (\attr -> attr{isSelection = False})
        modify (\attr -> attr{code = (code attr) ++ "L" ++ (show labelTT) ++ ":\n"})
        addTAC $ TACLabel $ "L" ++ (show labelTT)
        code_Stmt stmt1
        modify (\attr -> attr{code = (code attr) ++ " goto " ++ nextL ++ "\n"})
        addTAC $ TACGoto nextL
        modify (\attr -> attr{code = (code attr) ++ "L" ++ (show labelFF) ++ ":\n"})
        addTAC $ TACLabel $ "L" ++ (show labelFF)
        code_Stmt stmt2
        return ()

code_ComplexRExpr :: AbsNode -> State Attributes ()
code_ComplexRExpr (ComplexRExprNode _ complexRExpr) = case complexRExpr of
    Simple rExpr -> do
        code_RExpr rExpr
        return ()
    Array complexRExpr -> do
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
        modify (\attr -> attr{code = (code attr) ++ "if " ++ addr_RExpr1 ++ op ++ addr_RExpr2 ++ " goto " ++ tt ++ " goto " ++ ff ++ "\n"})
        modify (\attr -> attr{isSelection = True})
        addTAC $ TACIf (TACCondition addr_RExpr1 op addr_RExpr2) tt ff
        return ()
    OpAritm rExpr1 rExpr2 op -> do
        (code_RExpr rExpr1)
        addr_RExpr1 <- gets addr
        (code_RExpr rExpr2)
        addr_RExpr2 <- gets addr
        modify increaseCounterTemp
        modify (\attr -> attr{addr = "t" ++ (show $ counterTemp attr)})
        modify (\attr -> attr{code = (code attr) ++ (addr attr) ++ "=" ++ addr_RExpr1 ++ op ++ addr_RExpr2 ++ "\n"})
        addrAttr <- gets addr
        addTAC $ TACBinaryOp addrAttr addr_RExpr1 op addr_RExpr2
        return ()
    OpBoolean rExpr1 rExpr2 op -> do
        isSel <- gets isSelection
        if isSel
            then 
                case op of 
                "&&" -> do
                    (tt,ff) <- gets ttff
                    modify increaseCounterLab
                    label <- gets counterLab
                    modify (\attr -> attr{ttff = ("L" ++ (show label),ff)})
                    (code_RExpr rExpr1)
                    modify (\attr -> attr{code = (code attr) ++ "L" ++ (show label) ++ ":\n"})
                    addTAC $ TACLabel $ "L" ++ (show label)
                    modify (\attr -> attr{ttff = (tt,ff)})
                    (code_RExpr rExpr2)
                    return ()
                "||" -> do
                    (tt,ff) <- gets ttff
                    modify increaseCounterLab
                    label <- gets counterLab
                    modify (\attr -> attr{ttff = (tt,"L" ++ (show label))})
                    (code_RExpr rExpr1)
                    modify (\attr -> attr{code = (code attr) ++ "L" ++ (show label) ++ ":\n"})
                    addTAC $ TACLabel $ "L" ++ (show label)
                    modify (\attr -> attr{ttff = (tt,ff)})
                    (code_RExpr rExpr2)
                    return ()
            else    
                do
                (code_RExpr rExpr1)
                addr_RExpr1 <- gets addr
                (code_RExpr rExpr2)
                addr_RExpr2 <- gets addr
                modify increaseCounterTemp
                modify (\attr -> attr{addr = "t" ++ (show $ counterTemp attr)})
                modify (\attr -> attr{code = (code attr) ++ (addr attr) ++ "=" ++ addr_RExpr1 ++ op ++ addr_RExpr2 ++ "\n"})
                addrAttr <- gets addr
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
        modify increaseCounterTemp
        modify (\attr -> attr{addr = "t" ++ (show $ counterTemp attr)})
        modify (\attr -> attr{code = (code attr) ++ (addr attr) ++ "= negate" ++ addr_RExpr ++ "\n"})
        addrAttr <- gets addr
        addTAC $ TACUnaryOp addrAttr "-" addr_RExpr
        return ()
    Ref lExpr -> do
        return ()
    FCall funCall -> do
        code_FunCall funCall
        return ()
    Int integer -> do
        modify (\attr -> attr{addr = (show integer)})
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
                        modify (\attr -> attr{code = (code attr) ++ " goto " ++ tt ++ "\n"})
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
                        modify (\attr -> attr{code = (code attr) ++ " goto " ++ ff ++ "\n"})
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
        modify (\attr -> attr{code = (code attr) ++ addr_LExpr ++ " = " ++ addr_LExpr ++ " " ++ op ++ " 1" ++ "\n"})
        addTAC $ TACBinaryOp addr_LExpr addr_LExpr op "1"
        modify increaseCounterTemp
        modify (\attr -> attr{addr = "t" ++ (show $ counterTemp attr)})
        modify (\attr -> attr{code = (code attr) ++ (addr attr) ++ " = " ++ addr_LExpr ++ "\n"})
        addr_Attr <- gets addr
        addTAC $ TACAssign addr_Attr addr_LExpr
        return ()
    PostIncrDecr lExpr op -> do
        code_LExpr lExpr
        addr_LExpr <- gets addr
        modify increaseCounterTemp
        modify (\attr -> attr{addr = "t" ++ (show $ counterTemp attr)})
        modify (\attr -> attr{code = (code attr) ++ (addr attr) ++ " = " ++ addr_LExpr ++ "\n"})
        addr_Attr <- gets addr
        addTAC $ TACAssign addr_Attr addr_LExpr
        modify (\attr -> attr{code = (code attr) ++ addr_LExpr ++ " = " ++ addr_LExpr ++ " " ++ op ++ " 1" ++ "\n"})
        addTAC $ TACBinaryOp addr_LExpr addr_LExpr op "1"
        return ()
    BasLExpr bLExpr -> do
        code_BLExpr bLExpr
        return () 

code_BLExpr :: AbsNode -> State Attributes ()
code_BLExpr (BLExprNode _ bLExpr) = case bLExpr of
    ArrayEl bLExpr rExpr -> do 
        -- TODO gestire caso array
        return ()
    Id ident -> do
        modify (\attr -> attr{addr = (getIdent ident)})
        return ()




