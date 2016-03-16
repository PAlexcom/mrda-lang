module MRDACodeGenerator where

import MRDAParser
import qualified Data.Text as T

sanitizeFileName fileName = fileName ++ ".tac"

tacGenerator abstractSyntaxTree fileName = do
    print $ "Generating Code in filename " ++ (sanitizeFileName fileName)
    writeFile (sanitizeFileName fileName) $ code finalAttributes
    where
        finalAttributes = code_PROGRAM abstractSyntaxTree $ defaultAttributes 

data Attributes = Attributes {
    code :: String,
    env :: [Char],
    counter :: Int
}

defaultAttributes = Attributes "" [] 0 

-- expr = EXP
-- blk = BLOCK
-- prg = PROGRAM
-- decl = DECLARATION
-- str = String
-- int = Int
-- tps = Types
-- attr = Attributes
code_PROGRAM node attr = case node of
    Program blk prg -> Attributes code_attr env_attr counter_attr
        where
            blk_attr = (code_BLOCK blk attr)
            prg_attr = (code_PROGRAM prg attr)
            code_attr = (code blk_attr) ++ (code prg_attr)
            env_attr = (env attr)
            counter_attr = (counter attr)
    ProgramEmpty -> attr

code_BLOCK node attr = case node of
    BlockDecl decl -> Attributes {
            code = (code decl_attr),
            env = (env attr),
            counter = (counter attr) + 1
        }
        where
            decl_attr = (code_DECLARATION decl attr)
    BlockIf blkif -> Attributes {
            code = (code blkif_attr),
            env = (env attr),
            counter = (counter attr)
        }
        where
            blkif_attr = (code_BLOCKIF blkif attr)

code_BLOCKIF node attr = case node of
    StatementIfElse expr blk1 blk2 -> defaultAttributes
        where
            expr_attr = (code_EXP expr attr)
            blk1_attr = (code_BLOCK blk1 attr)
            blk2_attr = (code_BLOCK blk2 attr)
    StatementIf expr blk -> defaultAttributes
        where
            expr_attr = (code_EXP expr attr)
            blk_attr = (code_BLOCK blk attr)

code_DECLARATION node attr = case node of
    DeclarationExp tps str expr -> expr_attr
        where
            expr_attr = (code_EXP expr attr)

code_EXP node attr = case node of
    GrtOp expr1 expr2 -> defaultAttributes
    BoolGreaterOp expr1 expr2 -> defaultAttributes
    BoolLessOP expr1 expr2 -> defaultAttributes
    BoolEqualOp expr1 expr2 -> defaultAttributes
    BoolNEqOP expr1 expr2 -> defaultAttributes
    BoolGEqOP expr1 expr2 -> defaultAttributes
    BoolLEqOP expr1 expr2 -> defaultAttributes
    PlusOP expr1 expr2 -> Attributes code_attr env_attr counter_attr
        where
            expr1_attr = (code_EXP expr1 attr)
            expr2_attr = (code_EXP expr2 attr)
            code_attr = (code blkif_attr)
            env_attr = (env attr)
            counter_attr = (counter attr) + 1
    TimesOP expr1 expr2 -> defaultAttributes
        where
            expr1_attr = (code_EXP expr1 attr)
            expr2_attr = (code_EXP expr2 attr)
    Int int -> Attributes {
            code = (code attr) ++ (show int),
            env = (env attr),
            counter = (counter attr)
        }
    MinusOP expr1 expr2 -> defaultAttributes
    Bracket expr -> expr_attr
        where
            expr_attr = code_EXP expr attr
    NegateOP expr -> defaultAttributes
    DivideOP expr1 expr2 -> defaultAttributes
    TrueVal -> defaultAttributes
    FalseVal -> defaultAttributes
    Var str -> defaultAttributes

