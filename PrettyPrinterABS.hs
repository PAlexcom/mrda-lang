module PrettyPrinterABS where

import Parser
import Control.Monad.State


prettyPrintABS :: AbsNode -> String
prettyPrintABS abstractSyntaxTree = sourceCode $ execState (print_Node abstractSyntaxTree) defaultAttributes

data Attributes = Attributes {
    sourceCode :: String,
    counterTab :: Int
} deriving (Show)

defaultAttributes = Attributes "" 0

------------------------------------------------------------
--------- Enviroment Utilities -----------------------------
------------------------------------------------------------

increaseTab :: State Attributes ()
increaseTab = do
    modify (\attr -> attr{counterTab = (counterTab attr) + 1})
    return ()

decreaseTab :: State Attributes ()
decreaseTab = do
    modify (\attr -> attr{counterTab = (counterTab attr) - 1})
    return ()

appendCode :: String -> State Attributes ()
appendCode code = do
    modify (\attr -> attr{sourceCode = (sourceCode attr) ++ code})
    return ()

appendNewLineCode :: String -> State Attributes ()
appendNewLineCode code = do
    tabs <- gets counterTab
    modify (\attr -> attr{sourceCode = (sourceCode attr) ++ newlineSymbol ++ (tabSymbol tabs) ++ code})
    return ()

prependCode :: String -> State Attributes ()
prependCode code = do
    modify (\attr -> attr{sourceCode = code ++ (sourceCode attr)})
    return ()

------------------------------------------------------------
------------------------ Utilities -------------------------
------------------------------------------------------------

getIdent :: Ident -> String
getIdent (Ident ident) = ident

tabSymbol :: Int -> String
tabSymbol n
    | n <= 0 = ""
    | otherwise = "    " ++ (tabSymbol (n-1))

newlineSymbol :: String
newlineSymbol = "\n"

------------------------------------------------------------
---------------------- Pretty Printer ----------------------
------------------------------------------------------------

print_Program :: Program -> State Attributes ()
print_Program (Prog decls) = do
    print_NodeList decls
    return ()

print_Decl :: Decl -> State Attributes ()
print_Decl node = case node of
    DvarBInit modalityDeclNode ident basicTypeNode complexRExprNode -> do
        appendNewLineCode $ get_Node basicTypeNode
        appendCode " = "
        appendCode $ ";"
        return ()
    DvarCInit modalityDeclNode ident typeSpecNode complexRExprNode -> do
        return ()
    Dfun ident parametersNode basicTypeNode compStmtNode returnStmtNode -> do
        appendNewLineCode $ (get_Node basicTypeNode) ++ " " ++ (getIdent ident) ++ "(...) {"
        increaseTab
        print_Node compStmtNode
        print_Node returnStmtNode
        decreaseTab
        appendNewLineCode $ "}"
        return ()

print_ReturnStmt :: ReturnStmt -> State Attributes ()
print_ReturnStmt returnStmt = do
    appendNewLineCode "return"
    case returnStmt of
        RetExpVoid -> do
            return ()
        RetExp rExpr -> do
            appendCode " ("
            print_Node rExpr
            appendCode ")"
            return ()
    appendCode ";"
    return () 


print_CompStmt :: CompStmt -> State Attributes ()
print_CompStmt (BlockDecl decls stmts) = do
    print_NodeList decls
    print_NodeList stmts
    return ()

print_Stmt :: Stmt -> State Attributes ()
print_Stmt node = case node of
    Comp compStmt -> do
        print_Node compStmt
        return ()
    ProcCall funCall -> do
        print_Node funCall
        return ()
    Jmp jumpStmt -> do
        return ()
    Iter iterStmt -> do
        print_Node iterStmt
        return ()
    Sel selectionStmt -> do
        print_Node selectionStmt
        return ()
    Assgn lExpr assignment_op rExpr -> do
        return ()
    LExprStmt lExpr -> do
        print_Node lExpr
        return ()
    ExHandler tryCatchStmt -> do return()

print_FunCall :: FunCall -> State Attributes ()
print_FunCall (Call ident rExprs) = do
        return () 

print_IterStmt :: IterStmt -> State Attributes ()
print_IterStmt node = case node of
    While rExpr stmt -> do
        appendNewLineCode "while ("
        print_Node rExpr
        appendCode ") {"
        increaseTab
        print_Node stmt
        decreaseTab
        appendNewLineCode "}"
        return ()
    DoWhile stmt rExpr -> do
        return ()

print_SelectionStmt :: SelectionStmt -> State Attributes ()
print_SelectionStmt node = case node of
    IfNoElse rExpr stmt -> do
        appendNewLineCode "if ("
        print_Node rExpr
        appendCode ") {"
        increaseTab
        print_Node stmt
        decreaseTab
        appendCode "}"
        return ()
    IfElse rExpr stmt1 stmt2 -> do
        appendNewLineCode "if ("
        print_Node rExpr
        appendCode ") {"
        increaseTab
        print_Node stmt1
        decreaseTab
        appendNewLineCode "} else {"
        increaseTab
        print_Node stmt2
        decreaseTab
        appendNewLineCode "}"
        return ()

print_ComplexRExpr :: ComplexRExpr -> State Attributes ()
print_ComplexRExpr node = case node of
    Simple rExpr -> do
        print_Node rExpr
        return ()
    Array complexRExpr -> do
        return ()

print_RExpr :: RExpr -> State Attributes ()
print_RExpr node = case node of
    OpRelation rExpr1 rExpr2 op -> do
        print_Node rExpr1
        print_Node rExpr2
        return ()
    OpAritm rExpr1 rExpr2 op -> do
        print_Node rExpr1
        case op of
            "+" -> appendCode "+"
            "-" -> appendCode "-"
            "*" -> appendCode "*"
            "/" -> appendCode "/"
            "%" -> appendCode "%"
            "^" -> appendCode "^"
        print_Node rExpr2
        return ()
    OpBoolean rExpr1 rExpr2 op -> case op of 
        "&&" -> do
            print_Node rExpr1
            print_Node rExpr2
            return ()
        "||" -> do
            print_Node rExpr1
            print_Node rExpr2
            return ()
    Not rExpr -> do
        print_Node rExpr
        return ()
    Neg rExpr -> do
        print_Node rExpr
        return ()
    Ref lExpr -> do
        return ()
    FCall funCall -> do
        print_Node funCall
        return ()
    Int integer -> do
        appendCode $ show integer
        return ()
    Char char -> do
        return ()
    String string -> do
        appendCode $ show string
        return ()
    Float float -> do
        return ()
    Bool boolean -> 
        case boolean of
            Boolean_True -> do
                return ()
            Boolean_False -> do
                return ()
    Lexpr lExpr -> do
        print_Node lExpr
        return ()

print_LExpr :: LExpr -> State Attributes ()
print_LExpr node = case node of
    Deref rExpr -> do
        return ()
    PreIncrDecr lExpr symbol -> do
        print_Node lExpr
        return ()
    PostIncrDecr lExpr symbol -> do
        print_Node lExpr
        return ()
    BasLExpr bLExpr -> do
        print_Node bLExpr
        return () 

print_BLExpr :: BLExpr -> State Attributes ()
print_BLExpr bLExpr = case bLExpr of
    ArrayEl bLExpr rExpr -> do 
        return ()
    Id ident -> do
        appendCode $ getIdent ident
        return ()

get_BasicType :: BasicType -> String
get_BasicType (BType basicType) = basicType

------------------------------------------------------------
------------------ Pretty Printer Node ---------------------
------------------------------------------------------------

get_Node :: AbsNode -> String
get_Node node = case node of
    BasicTypeNode _ node -> get_BasicType node

print_Node :: AbsNode -> State Attributes ()
print_Node node = do
    case node of
        RExprNode _ node         -> print_RExpr node
        FunCallNode _ node       -> print_FunCall node
        LExprNode _ node         -> print_LExpr node
        BLExprNode _ node        -> print_BLExpr node
        ProgramNode _ node       -> print_Program node
        DeclNode _ node          -> print_Decl node
        TypeSpecNode _ node      -> do return()
        CompoundTypeNode _ node  -> do return()
        ComplexRExprNode _ node  -> print_ComplexRExpr node
        ParameterNode _ node     -> do return()
        ModalityParamNode _ node -> do return()
        ModalityDeclNode _ node  -> do return()
        CompStmtNode _ node      -> print_CompStmt node
        StmtNode _ node          -> print_Stmt node
        Assignment_opNode _ node -> do return()
        JumpStmtNode _ node      -> do return()
        ReturnStmtNode _ node    -> print_ReturnStmt node
        SelectionStmtNode _ node -> print_SelectionStmt node
        IterStmtNode _ node      -> do return()
        TryCatchStmtNode _ node -> do return()
    return ()

print_NodeList :: [AbsNode] -> State Attributes ()
print_NodeList (x:xs) = do
    print_Node x
    print_NodeList xs
    return ()

print_NodeList [] = do
    return ()