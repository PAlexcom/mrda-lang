module MRDACodeGenerator where

import MRDAParser
import Control.Monad.State

tacGenerator abstractSyntaxTree = do
    print $ "Generating Code in filename"
    print $ finalAttributes
    where
        finalAttributes = execState (code_PROGRAM abstractSyntaxTree) defaultAttributes 

data Attributes = Attributes {
    code :: String,
    env :: [Char],
    counter :: Int,
    addr :: String
} deriving (Show)

defaultAttributes = Attributes "" [] 0 ""

increaseCounter :: Attributes -> Attributes
increaseCounter attr = attr {counter = (counter attr) + 1}

-- expr = EXP
-- blk = BLOCK
-- prg = PROGRAM
-- decl = DECLARATION
-- str = String
-- int = Int
-- tps = Types
-- attr = Attributes
code_PROGRAM :: PROGRAM -> State Attributes ()
code_PROGRAM node = case node of
    Program blk prg -> do
            blk_attr <- (code_BLOCK blk)
            prg_attr <- (code_PROGRAM prg)
            return ()
    ProgramEmpty -> do
        return ()

code_BLOCK :: BLOCK -> State Attributes ()
code_BLOCK node = case node of
    BlockDecl decl -> do
            decl_attr <- (code_DECLARATION decl)
            return ()
    BlockIf blkif -> do
            blkif_attr <- (code_BLOCKIF blkif)
            return ()

code_BLOCKIF :: BLOCKIF -> State Attributes ()
code_BLOCKIF node = case node of
    StatementIfElse expr blk1 blk2 -> do
            expr_attr <- (code_EXP expr)
            blk1_attr <- (code_BLOCK blk1)
            blk2_attr <- (code_BLOCK blk2)
            return ()
    StatementIf expr blk -> do
            expr_attr <- (code_EXP expr)
            blk_attr <- (code_BLOCK blk)
            return ()

code_DECLARATION :: DECLARATION -> State Attributes ()
code_DECLARATION node = case node of
    DeclarationExp tps str expr -> do
        (code_EXP expr)
        expr_attr <- get
        modify increaseCounter
        modify (\attr -> attr{code = (code attr) ++ str ++ "=" ++ (addr expr_attr)})
        return ()

code_EXP :: EXP -> State Attributes ()
code_EXP node = case node of
    GrtOp expr1 expr2 -> do
        return ()
    BoolGreaterOp expr1 expr2 -> do
        return ()
    BoolLessOP expr1 expr2 -> do
        return ()
    BoolEqualOp expr1 expr2 -> do
        return ()
    BoolNEqOP expr1 expr2 -> do
        return ()
    BoolGEqOP expr1 expr2 -> do
        return ()
    BoolLEqOP expr1 expr2 -> do
        return ()
    PlusOP expr1 expr2 -> do
        (code_EXP expr1)
        expr1_attr <- get
        (code_EXP expr2)
        expr2_attr <- get
        modify increaseCounter
        modify (\attr -> attr{addr = "t" ++ (show $ counter attr)})
        modify (\attr -> attr{code = (code attr) ++ (addr attr) ++ "=" ++ (addr expr1_attr) ++ "+" ++ (addr expr2_attr)})
        return ()
    TimesOP expr1 expr2 -> do
        (code_EXP expr1)
        expr1_attr <- get
        (code_EXP expr2)
        expr2_attr <- get
        modify increaseCounter
        modify (\attr -> attr{addr = "t" ++ (show $ counter attr)})
        modify (\attr -> attr{code = (code attr) ++ (addr attr) ++ "=" ++ (addr expr1_attr) ++ "*" ++ (addr expr2_attr)})
        return ()
    Int int -> do
        modify (\attr -> attr{addr = (show int)})
        return ()
    MinusOP expr1 expr2 -> do
        (code_EXP expr1)
        expr1_attr <- get
        (code_EXP expr2)
        expr2_attr <- get
        modify increaseCounter
        modify (\attr -> attr{addr = "t" ++ (show $ counter attr)})
        modify (\attr -> attr{code = (code attr) ++ (addr attr) ++ "=" ++ (addr expr1_attr) ++ "-" ++ (addr expr2_attr)})
        return ()
    Bracket expr -> do
        expr_attr <- (code_EXP expr)
        return ()
    NegateOP expr -> do
        return ()
    DivideOP expr1 expr2 -> do
        (code_EXP expr1)
        expr1_attr <- get
        (code_EXP expr2)
        expr2_attr <- get
        modify increaseCounter
        modify (\attr -> attr{addr = "t" ++ (show $ counter attr)})
        modify (\attr -> attr{code = (code attr) ++ (addr attr) ++ "=" ++ (addr expr1_attr) ++ "/" ++ (addr expr2_attr)})
        return ()
    TrueVal -> do
        return ()
    FalseVal -> do
        return ()
    Var str -> do
        modify (\attr -> attr{addr = str})
        return ()



        

