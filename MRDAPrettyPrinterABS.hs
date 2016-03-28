module MRDAPrettyPrinterABS where

import MRDAParser
import Control.Monad.State


prettyPrintABS :: Program -> String
prettyPrintABS abstractSyntaxTree = sourceCode $ execState (print_Program abstractSyntaxTree) defaultAttributes

data Attributes = Attributes {
    sourceCode :: String,
    counterTab :: Int
} deriving (Show)

defaultAttributes = Attributes "" 0

------------------------------------------------------------
------------------------ Utilities -------------------------
------------------------------------------------------------

increaseTab :: State Attributes ()
increaseTab = do
    modify (\attr -> attr{counterTab = (counterTab attr) + 1})
    return ()

decreaseTab :: State Attributes ()
decreaseTab = do
    modify (\attr -> attr{counterTab = (counterTab attr) - 1})
    return ()

getIdent :: Ident -> String
getIdent (Ident ident) = ident

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
    print_Decls decls
    return ()

print_Decls :: [Decl] -> State Attributes ()
print_Decls (x:xs) = do
    print_Decl x
    print_Decls xs
    return ()

print_Decls [] = do
    return ()

print_Decl :: Decl -> State Attributes ()
print_Decl node = case node of
    DvarBInit basicType varDeclInits -> do
        appendNewLineCode $ get_BasicType basicType
        appendCode " = "
        print_VarDeclInits varDeclInits
        appendCode $ ";"
        return ()
    DvarCInit _ varDeclInits -> do
        print_VarDeclInits varDeclInits
        return ()
    Dfun basicType ident parameters compStmt returnStmt -> do
        appendNewLineCode $ (get_BasicType basicType) ++ " " ++ (getIdent ident) ++ "(...) {"
        increaseTab
        print_CompStmt compStmt
        print_ReturnStmt returnStmt
        decreaseTab
        appendNewLineCode $ "}"
        return ()

get_BasicType :: BasicType -> String
get_BasicType (BType basicType) = basicType  

print_ReturnStmt :: ReturnStmt -> State Attributes ()
print_ReturnStmt returnStmt = do
    appendNewLineCode "return"
    case returnStmt of
        RetExpVoid -> do
            return ()
        RetExp rExpr -> do
            appendCode " ("
            print_RExpr rExpr
            appendCode ")"
            return ()
    appendCode ";"
    return () 


print_CompStmt :: CompStmt -> State Attributes ()
print_CompStmt (BlockDecl decls stmts) = do
    print_Decls decls
    print_Stmts stmts
    return ()

print_Stmts :: [Stmt] -> State Attributes ()
print_Stmts [] = do
    return ()

print_Stmts (x:xs) = do
    print_Stmt x
    print_Stmts xs
    return ()

print_Stmt :: Stmt -> State Attributes ()
print_Stmt node = case node of
    Comp compStmt -> do
        print_CompStmt compStmt
        return ()
    ProcCall funCall -> do
        print_FunCall funCall
        return ()
    Jmp jumpStmt -> do
        return ()
    Iter iterStmt -> do
        print_IterStmt iterStmt
        return ()
    Sel selectionStmt -> do
        print_SelectionStmt selectionStmt
        return ()
    Assgn lExpr assignment_op rExpr -> do
        return ()
    LExprStmt lExpr -> do
        print_LExpr lExpr
        return ()

print_FunCall :: FunCall -> State Attributes ()
print_FunCall (Call ident rExprs) = do
        return () 

print_IterStmt :: IterStmt -> State Attributes ()
print_IterStmt node = case node of
    While rExpr stmt -> do
        appendNewLineCode "while ("
        print_RExpr rExpr
        appendCode ") {"
        increaseTab
        print_Stmt stmt
        decreaseTab
        appendNewLineCode "}"
        return ()
    DoWhile stmt rExpr -> do
        return ()

print_SelectionStmt :: SelectionStmt -> State Attributes ()
print_SelectionStmt node = case node of
    IfNoElse rExpr stmt -> do
        appendNewLineCode "if ("
        print_RExpr rExpr
        appendCode ") {"
        increaseTab
        print_Stmt stmt
        decreaseTab
        appendCode "}"
        return ()
    IfElse rExpr stmt1 stmt2 -> do
        appendNewLineCode "if ("
        print_RExpr rExpr
        appendCode ") {"
        increaseTab
        print_Stmt stmt1
        decreaseTab
        appendNewLineCode "} else {"
        increaseTab
        print_Stmt stmt2
        decreaseTab
        appendNewLineCode "}"
        return ()

print_VarDeclInits :: [VarDeclInit] -> State Attributes ()
print_VarDeclInits [] = do
    return ()

print_VarDeclInits (x:xs) = do
    print_VarDeclInit x
    print_VarDeclInits xs
    return ()

print_VarDeclInit :: VarDeclInit -> State Attributes ()
print_VarDeclInit (VarDeclIn ident complexRExpr) = do
    (print_ComplexRExpr complexRExpr)
    return ()

print_ComplexRExpr :: ComplexRExpr -> State Attributes ()
print_ComplexRExpr node = case node of
    Simple rExpr -> do
        print_RExpr rExpr
        return ()
    Array complexRExpr -> do
        return ()

print_RExpr :: RExpr -> State Attributes ()
print_RExpr node = case node of
    OpRelation rExpr1 rExpr2 op -> do
        (print_RExpr rExpr1)
        (print_RExpr rExpr2)
        return ()
    OpAritm rExpr1 rExpr2 op -> do
        print_RExpr rExpr1
        case op of
            "+" -> do
                appendCode "+"
                return ()
            "-" -> do
                appendCode "-"
                return ()
            "*" -> do
                appendCode "*"
                return ()
            "/" -> do
                appendCode "/"
                return ()
            "%" -> do
                appendCode "%"
                return ()
            "^" -> do
                appendCode "^"
                return ()
        print_RExpr rExpr2
        return ()
    OpBoolean rExpr1 rExpr2 op -> case op of 
        "&&" -> do
            print_RExpr rExpr1
            print_RExpr rExpr2
            return ()
        "||" -> do
            (print_RExpr rExpr1)
            (print_RExpr rExpr2)
            return ()
    Not rExpr -> do
        print_RExpr rExpr
        return ()
    Neg rExpr -> do
        (print_RExpr rExpr)
        return ()
    Ref lExpr -> do
        return ()
    FCall funCall -> do
        print_FunCall funCall
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
        print_LExpr lExpr
        return ()

print_LExpr :: LExpr -> State Attributes ()
print_LExpr node = case node of
    Deref rExpr -> do
        return ()
    PreInc lExpr -> do
        print_LExpr lExpr
        return ()
    PreDecr lExpr -> do
        print_LExpr lExpr
        return ()
    PostInc lExpr -> do
        return ()
    PostDecr lExpr -> do
        return ()
    BasLExpr bLExpr -> do
        print_BLExpr bLExpr
        return () 

print_BLExpr :: BLExpr -> State Attributes ()
print_BLExpr bLExpr = case bLExpr of
    ArrayEl bLExpr rExpr -> do 
        return ()
    Id ident -> do
        appendCode $ getIdent ident
        return ()




