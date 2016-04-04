{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module PrettyPrinterABS where

import Parser
--import Control.Monad.State


--prettyPrintABS :: AbsNode -> String
--prettyPrintABS abstractSyntaxTree = sourceCode $ execState (print_Node abstractSyntaxTree) defaultAttributes

--data Attributes = Attributes {
--    sourceCode :: String,
--    counterTab :: Int
--} deriving (Show)

--defaultAttributes = Attributes "" 0

--------------------------------------------------------------
----------- Enviroment Utilities -----------------------------
--------------------------------------------------------------

--increaseTab :: State Attributes ()
--increaseTab = do
--    modify (\attr -> attr{counterTab = (counterTab attr) + 1})
--    return ()

--decreaseTab :: State Attributes ()
--decreaseTab = do
--    modify (\attr -> attr{counterTab = (counterTab attr) - 1})
--    return ()

--appendCode :: String -> State Attributes ()
--appendCode code = do
--    modify (\attr -> attr{sourceCode = (sourceCode attr) ++ code})
--    return ()

--appendNewLineCode :: String -> State Attributes ()
--appendNewLineCode code = do
--    tabs <- gets counterTab
--    modify (\attr -> attr{sourceCode = (sourceCode attr) ++ newlineSymbol ++ (tabSymbol tabs) ++ code})
--    return ()

--prependCode :: String -> State Attributes ()
--prependCode code = do
--    modify (\attr -> attr{sourceCode = code ++ (sourceCode attr)})
--    return ()

--------------------------------------------------------------
-------------------------- Utilities -------------------------
--------------------------------------------------------------

--getIdent :: Ident -> String
--getIdent (Ident ident) = ident

--tabSymbol :: Int -> String
--tabSymbol n
--    | n <= 0 = ""
--    | otherwise = "    " ++ (tabSymbol (n-1))

--newlineSymbol :: String
--newlineSymbol = "\n"

--------------------------------------------------------------
------------------------ Pretty Printer ----------------------
--------------------------------------------------------------

--print_Program :: Program -> State Attributes ()
--print_Program (Prog decls) = do
--    print_NodeList decls
--    return ()

--print_Decl :: Decl -> State Attributes ()
--print_Decl node = case node of
--    DvarBInit modalityDeclNode ident basicTypeNode complexRExprNode -> do
--        appendNewLineCode $ get_Node basicTypeNode
--        appendCode " = "
--        appendCode $ ";"
--        return ()
--    DvarCInit modalityDeclNode ident typeSpecNode complexRExprNode -> do
--        return ()
--    Dfun ident parametersNode basicTypeNode compStmtNode returnStmtNode -> do
--        appendNewLineCode $ (get_Node basicTypeNode) ++ " " ++ (getIdent ident) ++ "(...) {"
--        increaseTab
--        print_Node compStmtNode
--        print_Node returnStmtNode
--        decreaseTab
--        appendNewLineCode $ "}"
--        return ()

--print_ReturnStmt :: ReturnStmt -> State Attributes ()
--print_ReturnStmt returnStmt = do
--    appendNewLineCode "return"
--    case returnStmt of
--        RetExpVoid -> do
--            return ()
--        RetExp rExpr -> do
--            appendCode " ("
--            print_Node rExpr
--            appendCode ")"
--            return ()
--    appendCode ";"
--    return () 


--print_CompStmt :: CompStmt -> State Attributes ()
--print_CompStmt (BlockDecl decls stmts) = do
--    print_NodeList decls
--    print_NodeList stmts
--    return ()

--print_Stmt :: Stmt -> State Attributes ()
--print_Stmt node = case node of
--    Comp compStmt -> do
--        print_Node compStmt
--        return ()
--    ProcCall funCall -> do
--        print_Node funCall
--        return ()
--    Jmp jumpStmt -> do
--        return ()
--    Iter iterStmt -> do
--        print_Node iterStmt
--        return ()
--    Sel selectionStmt -> do
--        print_Node selectionStmt
--        return ()
--    Assgn lExpr assignment_op rExpr -> do
--        return ()
--    LExprStmt lExpr -> do
--        print_Node lExpr
--        return ()
--    ExHandler tryCatchStmt -> do return()

--print_FunCall :: FunCall -> State Attributes ()
--print_FunCall (Call ident rExprs) = do
--        return () 

--print_IterStmt :: IterStmt -> State Attributes ()
--print_IterStmt node = case node of
--    While rExpr stmt -> do
--        appendNewLineCode "while ("
--        print_Node rExpr
--        appendCode ") {"
--        increaseTab
--        print_Node stmt
--        decreaseTab
--        appendNewLineCode "}"
--        return ()

--print_SelectionStmt :: SelectionStmt -> State Attributes ()
--print_SelectionStmt node = case node of
--    IfNoElse rExpr stmt -> do
--        appendNewLineCode "if ("
--        print_Node rExpr
--        appendCode ") {"
--        increaseTab
--        print_Node stmt
--        decreaseTab
--        appendCode "}"
--        return ()
--    IfElse rExpr stmt1 stmt2 -> do
--        appendNewLineCode "if ("
--        print_Node rExpr
--        appendCode ") {"
--        increaseTab
--        print_Node stmt1
--        decreaseTab
--        appendNewLineCode "} else {"
--        increaseTab
--        print_Node stmt2
--        decreaseTab
--        appendNewLineCode "}"
--        return ()

--print_ComplexRExpr :: ComplexRExpr -> State Attributes ()
--print_ComplexRExpr node = case node of
--    Simple rExpr -> do
--        print_Node rExpr
--        return ()
--    Array complexRExpr -> do
--        return ()

--print_RExpr :: RExpr -> State Attributes ()
--print_RExpr node = case node of
--    OpRelation rExpr1 rExpr2 op -> do
--        print_Node rExpr1
--        print_Node rExpr2
--        return ()
--    OpAritm rExpr1 rExpr2 op -> do
--        print_Node rExpr1
--        case op of
--            "+" -> appendCode "+"
--            "-" -> appendCode "-"
--            "*" -> appendCode "*"
--            "/" -> appendCode "/"
--            "%" -> appendCode "%"
--            "^" -> appendCode "^"
--        print_Node rExpr2
--        return ()
--    OpBoolean rExpr1 rExpr2 op -> case op of 
--        "&&" -> do
--            print_Node rExpr1
--            print_Node rExpr2
--            return ()
--        "||" -> do
--            print_Node rExpr1
--            print_Node rExpr2
--            return ()
--    Not rExpr -> do
--        print_Node rExpr
--        return ()
--    Neg rExpr -> do
--        print_Node rExpr
--        return ()
--    Ref lExpr -> do
--        return ()
--    FCall funCall -> do
--        print_Node funCall
--        return ()
--    Int integer -> do
--        appendCode $ show integer
--        return ()
--    Char char -> do
--        return ()
--    String string -> do
--        appendCode $ show string
--        return ()
--    Float float -> do
--        return ()
--    Bool boolean -> 
--        case boolean of
--            Boolean_True -> do
--                return ()
--            Boolean_False -> do
--                return ()
--    Lexpr lExpr -> do
--        print_Node lExpr
--        return ()

--print_LExpr :: LExpr -> State Attributes ()
--print_LExpr node = case node of
--    Deref rExpr -> do
--        return ()
--    PreIncrDecr lExpr symbol -> do
--        print_Node lExpr
--        return ()
--    PostIncrDecr lExpr symbol -> do
--        print_Node lExpr
--        return ()
--    BasLExpr bLExpr -> do
--        print_Node bLExpr
--        return () 

--print_BLExpr :: BLExpr -> State Attributes ()
--print_BLExpr bLExpr = case bLExpr of
--    ArrayEl bLExpr rExpr -> do 
--        return ()
--    Id ident -> do
--        appendCode $ getIdent ident
--        return ()

--get_BasicType :: BasicType -> String
--get_BasicType (BType basicType) = basicType

--------------------------------------------------------------
-------------------- Pretty Printer Node ---------------------
--------------------------------------------------------------

--get_Node :: AbsNode -> String
--get_Node node = case node of
--    BasicTypeNode _ node -> get_BasicType node

--print_Node :: AbsNode -> State Attributes ()
--print_Node node = do
--    case node of
--        RExprNode _ node         -> print_RExpr node
--        FunCallNode _ node       -> print_FunCall node
--        LExprNode _ node         -> print_LExpr node
--        BLExprNode _ node        -> print_BLExpr node
--        ProgramNode _ node       -> print_Program node
--        DeclNode _ node          -> print_Decl node
--        TypeSpecNode _ node      -> do return()
--        CompoundTypeNode _ node  -> do return()
--        ComplexRExprNode _ node  -> print_ComplexRExpr node
--        ParameterNode _ node     -> do return()
--        ModalityParamNode _ node -> do return()
--        ModalityDeclNode _ node  -> do return()
--        CompStmtNode _ node      -> print_CompStmt node
--        StmtNode _ node          -> print_Stmt node
--        Assignment_opNode _ node -> do return()
--        JumpStmtNode _ node      -> do return()
--        ReturnStmtNode _ node    -> print_ReturnStmt node
--        SelectionStmtNode _ node -> print_SelectionStmt node
--        IterStmtNode _ node      -> do return()
--        TryCatchStmtNode _ node -> do return()
--    return ()

--print_NodeList :: [AbsNode] -> State Attributes ()
--print_NodeList (x:xs) = do
--    print_Node x
--    print_NodeList xs
--    return ()

--print_NodeList [] = do
--    return ()

--------------- PROVA ---------------
import Text.PrettyPrint hiding (Str)

tab = 5

instance PrettyPrinter Program where
    pPrt (Prog decls) = vcat . map (pPrt $ decl) decls

instance PrettyPrinter Decl where
    pPrt (DvarBInit modD ident basType cRExpr) =
        pPrt (modalityDecl modD) <+> pPrt ident <> colon <+> pPrt (basicType basType) <+> text "=" <+> pPrt (complexRExpr cRExpr)
    pPrt (DvarCInit modD ident tpSpec cRExpr) =
        pPrt (modalityDecl modD) <+> pPrt ident <> colon <+> pPrt (typeSpec tpSpec) <+> text "=" <+> pPrt (complexRExpr cRExpr)
    pPrt (Dfun ident params basType cStmt rStmt) =
        text "def" <+> pPrt ident <+> (parens (vcat . map (pPrt $ parameter) params)) <> colon <> pPrt (basicType basType) <+> text "=" <+> braces $$ pPrt (compStmt cStmt) $$ pPrt (returnStmt rStmt)

instance PrettyPrinter ModalityDecl where
    pPrt ModalityD_val = text "val" 
    pPrt ModalityD_var = text "var"

instance PrettyPrinter ModalityParam where
    pPrt ModalityPEmpty = text ""
    pPrt ModalityP_val = text "val"
    pPrt ModalityP_var = text "var"
    pPrt ModalityP_valres = text "valres"

instance PrettyPrinter Ident where
    pPrt (Ident ident) = text ident

instance PrettyPrinter Int where
    pPrt int = text (show int) 

instance PrettyPrinter Char where
    pPrt char = text (show char) 

instance PrettyPrinter String where
    pPrt string = text string 

instance PrettyPrinter Float where
    pPrt float = text (show float) 

instance PrettyPrinter Bool where
    pPrt bool = text (show bool) 

instance PrettyPrinter BasicType where
    pPrt (BType tp) = text tp

instance PrettyPrinter TypeSpec where
    pPrt (BasTyp basType) = pPrt (basicType basType)
    pPrt (CompType compType) = pPrt (compoundType compType)

instance PrettyPrinter CompoundType where
    pPrt (ArrDef tpSpec int) = text "Array" <> (brackets (pPrt (typeSpec tpSpec))) <> (parens (pPrt int))
    pPrt (Pointer tpSpec) = text "*" <> pPrt (typeSpec tpSpec)

instance PrettyPrinter ComplexRExpr where
    pPrt (Simple rExp) = pPrt (rExpr rExp)
    pPrt (Array cRexprs) = text "Array" <> (parens (vcat . map (pPrt $ complexRExpr) cRexprs))

instance PrettyPrinter Parameter where
    pPrt (Param modP ident tpSpec) = pPrt (modalityParam modP) <+> pPrt ident <> colon <+> pPrt (typeSpec tpSpec)

instance PrettyPrinter RExpr where
    pPrt (OpRelation rExp1 rExp2 op) = pPrt (rExpr rExp1) <+> text op <+> pPrt(rExpr rExp2)
    pPrt (OpAritm rExp1 rExp2 op) = pPrt (rExpr rExp1) <+> text op <+> pPrt(rExpr rExp2)
    pPrt (OpBoolean rExp1 rExp2 op) = pPrt (rExpr rExp1) <+> text op <+> pPrt(rExpr rExp2)
    pPrt (Not rExp) = text "!" <> pPrt (rExpr rExp) 
    pPrt (Neg rExp) = text "-" <> pPrt (rExpr rExp)
    pPrt (Ref lExp) = text "&" <> pPrt (lExpr lExp)
    pPrt (FCall fcall) = pPrt (funCall fcall)
    pPrt (Int int) = pPrt int
    pPrt (Char char) = pPrt char
    pPrt (String string) = pPrt string
    pPrt (Float float) = pPrt float
    pPrt (Bool bool) = pPrt bool
    pPrt (Lexpr lExp) = pPrt (lExpr lExp)

instance PrettyPrinter LExpr where
    pPrt (Deref rExp) = text "*" <> pPrt (rExpr rExp)
    pPrt (PreIncrDecr lExp op) = text op <> text op <> pPrt (lExpr lExp)
    pPrt (PostIncrDecr lExp op) = pPrt (lExpr lExp) <> text op <> text op
    pPrt (BasLExpr bLExp) = pPrt (bLExpr bLExp)

instance PrettyPrinter FunCall where
    pPrt (Call ident rExprs) = pPrt ident <> (parens (vcat . map (pPrt $ rExpr) rExprs)) 

instance PrettyPrinter BLExpr where
    pPrt (ArrayEl bLExp rExp) = pPrt (bLExpr bLExp) <> (brackets (pPrt (rExpr rExp)))
    pPrt (Id ident) = pPrt ident 

instance PrettyPrinter CompStmt where
    pPrt (BlockDecl decls stmts) = (vcat . map (pPrt $ decl) decls) $$ (vcat . map (pPrt $ stmt) stmts)

instance PrettyPrinter Stmt where
    pPrt (Comp cStmt) = (braces ($$ pPrt (compStmt cStmt))) 
    pPrt (ProcCall fcall) = pPrt (funCall fcall) <> semi
    pPrt (Jmp jStmt) = pPrt (jumpStmt jStmt) <> semi
    pPrt (Iter iStmt) = pPrt (iterStmt iStmt) 
    pPrt (Sel sStmt) = pPrt (selectionStmt sStmt)
    pPrt (Assgn lExp op rExp) = pPrt (lExpr lExp) <+> pPrt op <+> pPrt (rExpr rExp) <> semi
    pPrt (LExprStmt lExp) = pPrt (lExpr lExp) <> semi 
    pPrt (ExHandler tcStmt) = pPrt (tryCatch tcStmt)

instance PrettyPrinter TryCatchStmt where
    pPrt (TryCatch stm1 ident stm2) = text "try" <+> pPrt (stmt stm1) <+> text "catch" <+> (braces <+> text "case" <+> text "ex" <> colon <+> pPrt ident <+> text "=>" <+> pPrt (stmt stm2) )

instance PrettyPrinter Assignment_op where
    pPrt Assign = text "="
    pPrt (AssignOp op) = text op <> text "="

instance PrettyPrinter JumpStmt where
    pPrt Break = text "break"
    pPrt Continue = text "continue"

instance PrettyPrinter ReturnStmt where
    pPrt RetExpVoid = text "return" <> semi
    pPrt (RetExp rExp) = text "return" <> (parens (pPrt (rExpr rExp))) <> semi

instance PrettyPrinter SelectionStmt where
    pPrt (IfNoElse rExp stm) = text "if" <> (parens (pPrt (rExpr rExp))) <+> pPrt (stmt stm)  
    pPrt (IfElse rExp stm1 stm2) = text "if" <> (parens (pPrt (rExpr rExp))) <+> pPrt (stmt stm1) <+> text "else" <+> pPrt (stmt stm2)

instance PrettyPrinter IterStmt where
    pPrt (While rExp stm) = text "while" <> (parens (pPrt (rExpr rExp))) <+> pPrt (stmt stm) 
    pPrt (For ident rExp1 rExp2 stm) = text "for" <> (parens ((pPrt Ident) <+> text "<-" <+> (pPrt (rExpr rExp1)) <+> text "to" <+> (pPrt (rExpr rExp2)))) <+> pPrt (stmt stm)

class PrettyPrinter a where
  pPrt :: a -> Doc
