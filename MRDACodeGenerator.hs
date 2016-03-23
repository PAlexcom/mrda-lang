module MRDACodeGenerator where

import MRDAParser
import Control.Monad.State

tacGenerator abstractSyntaxTree = execState (code_Program abstractSyntaxTree) defaultAttributes

data Attributes = Attributes {
    code :: String,
    env :: [Char],
    counterTemp :: Int,
    counterLab :: Int,
    addr :: String,
    ttff :: (String,String),
    next :: String,
    isSelection :: Bool
} deriving (Show)

defaultAttributes = Attributes "" [] 0 0 "" ("","") "" False

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

code_Program :: Program -> State Attributes ()
code_Program (Prog decls) = do
    modify increaseCounterLab
    label <- gets counterLab
    code_Decls decls
    modify (\attr -> attr{code = (code attr) ++ "L" ++ (show label) ++ ":halt" ++ "\n"})
    return ()

code_Decls :: [Decl] -> State Attributes ()
code_Decls (x:xs) = do
    code_Decl x
    code_Decls xs
    return ()

code_Decls [] = do
    return ()

code_Decl :: Decl -> State Attributes ()
code_Decl node = case node of
    DvarBInit _ varDeclInits -> do
        code_VarDeclInits varDeclInits
        return ()
    DvarCInit _ varDeclInits -> do
        code_VarDeclInits varDeclInits
        return ()
    Dfun _ ident parameters compStmt -> do
        modify (\attr -> attr{code = (code attr) ++ (getIdent ident) ++ "BeginFunc" ++ "\n"})
        code_CompStmt compStmt
        modify (\attr -> attr{code = (code attr) ++ "EndFunc" ++ "\n"})
        return ()

code_CompStmt :: CompStmt -> State Attributes ()
code_CompStmt (BlockDecl decls stmts) = do
    code_Decls decls
    next <- gets next
    code_Stmts stmts next
    modify (\attr -> attr{next = next})
    return ()

code_Stmts :: [Stmt] -> String -> State Attributes ()
code_Stmts [x] next = do
    modify (\attr -> attr{next = next})
    code_Stmt x
    return ()

code_Stmts (x:xs) next = do
    modify increaseCounterLab
    label <- gets counterLab
    modify (\attr -> attr{next = ("L" ++ (show label))})
    code_Stmt x
    modify (\attr -> attr{code = (code attr) ++ "L" ++ (show label) ++ "\n"})
    code_Stmts xs next
    return ()

code_Stmts [] _ = do
    return ()

code_Stmt :: Stmt -> State Attributes ()
code_Stmt node = case node of
    Comp compStmt -> do
        code_CompStmt compStmt
        return ()
    ProcCall funCall -> do

        return ()
    Jmp jumpStmt -> do
        return ()
    Iter iterStmt -> do
        code_IterStmt iterStmt
        return ()
    Sel selectionStmt -> do
        code_SelectionStmt selectionStmt
        return ()
    Assgn lExpr assignment_op rExpr -> do
        return ()
    LExprStmt lExpr -> do
        return ()

code_IterStmt :: IterStmt -> State Attributes ()
code_IterStmt node = case node of
    While rExpr stmt -> do
        next <- gets next
        modify increaseCounterLab
        begin <- gets counterLab
        modify increaseCounterLab
        labelTT <- gets counterLab
        modify (\attr -> attr{ttff = ("L" ++ (show labelTT), next)})
        modify (\attr -> attr{code = (code attr) ++ "L" ++ (show begin) ++ "\n"})
        modify (\attr -> attr{isSelection = True})
        code_RExpr rExpr
        modify (\attr -> attr{isSelection = False})
        modify (\attr -> attr{code = (code attr) ++ "L" ++ (show labelTT) ++ "\n"})
        modify (\attr -> attr{next = ("L" ++ (show begin))})
        code_Stmt stmt
        modify (\attr -> attr{code = (code attr) ++ "goto L" ++ (show labelTT) ++ "\n"})
        return ()
    DoWhile stmt rExpr -> do
        return ()

code_SelectionStmt :: SelectionStmt -> State Attributes ()
code_SelectionStmt node = case node of
    IfNoElse rExpr stmt -> do
        next <- gets next
        modify increaseCounterLab
        label <- gets counterLab
        modify (\attr -> attr{ttff = ("L" ++ (show label), next)})
        modify (\attr -> attr{isSelection = True})
        code_RExpr rExpr
        modify (\attr -> attr{isSelection = False})
        modify (\attr -> attr{code = (code attr) ++ "L" ++ (show label) ++ "\n"})
        code_Stmt stmt
        return ()
    IfElse rExpr stmt1 stmt2 -> do
        next <- gets next
        modify increaseCounterLab
        labelTT <- gets counterLab
        modify increaseCounterLab
        labelFF <- gets counterLab
        modify (\attr -> attr{ttff = ("L" ++ (show labelTT), "L" ++ (show labelFF))})
        modify (\attr -> attr{isSelection = True})
        code_RExpr rExpr
        modify (\attr -> attr{isSelection = False})
        modify (\attr -> attr{code = (code attr) ++ "L" ++ (show labelTT) ++ "\n"})
        code_Stmt stmt1
        modify (\attr -> attr{code = (code attr) ++ "goto " ++ next ++ "\n"})
        modify (\attr -> attr{code = (code attr) ++ "L" ++ (show labelFF) ++ "\n"})
        code_Stmt stmt2
        return ()

code_VarDeclInits :: [VarDeclInit] -> State Attributes ()
code_VarDeclInits [] = do
    return ()

code_VarDeclInits (x:xs) = do
    code_VarDeclInit x
    code_VarDeclInits xs
    return ()

code_VarDeclInit :: VarDeclInit -> State Attributes ()
code_VarDeclInit (VarDeclIn ident complexRExpr) = do
    (code_ComplexRExpr complexRExpr)
    expr_attr <- get
    modify (\attr -> attr{code = (code attr) ++ (getIdent ident) ++ "=" ++ (addr expr_attr) ++ "\n"})

code_ComplexRExpr :: ComplexRExpr -> State Attributes ()
code_ComplexRExpr node = case node of
    Simple rExpr -> do
                code_RExpr rExpr
                return ()
    Array complexRExpr -> do
                return ()

code_RExpr :: RExpr -> State Attributes ()
code_RExpr node = case node of
    OpRelation rExpr1 rExpr2 op -> do
        (tt,ff) <- gets ttff
        (code_RExpr rExpr1)
        addr_RExpr1 <- gets addr
        (code_RExpr rExpr2)
        addr_RExpr2 <- gets addr
        modify (\attr -> attr{code = (code attr) ++ "if " ++ addr_RExpr1 ++ op ++ addr_RExpr2 ++ "goto " ++ tt ++ "goto " ++ ff ++ "\n"})
        return ()
    OpAritm rExpr1 rExpr2 op -> do
        (code_RExpr rExpr1)
        addr_RExpr1 <- gets addr
        (code_RExpr rExpr2)
        addr_RExpr2 <- gets addr
        modify increaseCounterTemp
        modify (\attr -> attr{addr = "t" ++ (show $ counterTemp attr)})
        modify (\attr -> attr{code = (code attr) ++ (addr attr) ++ "=" ++ addr_RExpr1 ++ op ++ addr_RExpr2 ++ "\n"})
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
                    modify (\attr -> attr{code = (code attr) ++ "L" ++ (show label) ++ "\n"})
                    modify (\attr -> attr{ttff = (tt,ff)})
                    (code_RExpr rExpr2)
                    return ()
                "||" -> do
                    (tt,ff) <- gets ttff
                    modify increaseCounterLab
                    label <- gets counterLab
                    modify (\attr -> attr{ttff = (tt,"L" ++ (show label))})
                    (code_RExpr rExpr1)
                    modify (\attr -> attr{code = (code attr) ++ "L" ++ (show label) ++ "\n"})
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
        return ()
    Ref lExpr -> do
        return ()
    FCall funCall -> do

        return ()
    Int integer -> do
        modify (\attr -> attr{addr = (show integer)})
        return ()
    Char char -> do
        modify (\attr -> attr{addr = (char:[])})
        return ()
    String string -> do
        modify (\attr -> attr{addr = string})
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
                        modify (\attr -> attr{code = (code attr) ++ "goto " ++ tt ++ "\n"})
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
                        modify (\attr -> attr{code = (code attr) ++ "goto " ++ ff ++ "\n"})
                        return ()
                    else
                        do
                        modify (\attr -> attr{addr = "False"})
                        return ()
    Lexpr lExpr -> do
        code_LExpr lExpr
        addr_LExpr <- gets addr
        modify increaseCounterTemp
        modify (\attr -> attr{addr = "t" ++ (show $ counterTemp attr)})
        modify (\attr -> attr{code = (code attr) ++ (addr attr) ++ "=" ++ addr_LExpr ++ "\n"})
        return ()

code_LExpr :: LExpr -> State Attributes ()
code_LExpr node = case node of
    Deref rExpr -> do
        return ()
    PreInc lExpr -> do
        code_LExpr lExpr
        addr_LExpr <- gets addr
        modify increaseCounterTemp
        modify (\attr -> attr{addr = "t" ++ (show $ counterTemp attr)})
        modify (\attr -> attr{code = (code attr) ++ (addr attr) ++ "=" ++ addr_LExpr ++ "+ 1" ++ "\n"})
        return ()
    PreDecr lExpr -> do
        code_LExpr lExpr
        addr_LExpr <- gets addr
        modify increaseCounterTemp
        modify (\attr -> attr{addr = "t" ++ (show $ counterTemp attr)})
        modify (\attr -> attr{code = (code attr) ++ (addr attr) ++ "=" ++ addr_LExpr ++ "- 1" ++ "\n"})
        return ()
    PostInc lExpr -> do
        return ()
    PostDecr lExpr -> do
        return ()
    BasLExpr bLExpr -> do
        code_BLExpr bLExpr
        return () 

code_BLExpr :: BLExpr -> State Attributes ()
code_BLExpr bLExpr = case bLExpr of
    ArrayEl bLExpr rExpr -> do 
        -- TODO gestire caso array
        return ()
    Id ident -> do
        modify (\attr -> attr{addr = (getIdent ident)})
        return ()




