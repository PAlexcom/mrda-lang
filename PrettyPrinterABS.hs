{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module PrettyPrinterABS where

import Parser

import Text.PrettyPrint

tab = 2

instance PrettyPrinter Program where
    pPrt (Prog decls) = vcat ( map pPrt (map gDecl decls))

instance PrettyPrinter Decl where
    pPrt (DvarBInit modD ident basType cRExpr) =
        pPrt (gModalityDecl modD) <+> pPrt ident <> colon <+> pPrt (gBasicType basType) <+> text "=" <+> pPrt (gComplexRExpr cRExpr) <> semi
    pPrt (DvarCInit modD ident tpSpec cRExpr) =
        pPrt (gModalityDecl modD) <+> pPrt ident <> colon <+> pPrt (gTypeSpec tpSpec) <+> text "=" <+> pPrt (gComplexRExpr cRExpr) <> semi
    pPrt (Dfun ident params basType cStmt rStmt) =
        text "def" <+> pPrt ident <+> (parens (hsep (punctuate comma (map pPrt (map gParameter params))))) <> colon <> pPrt (gBasicType basType) <+> text "=" <+> lbrace $$ nest tab (pPrt (gCompStmt cStmt) $+$ pPrt (gReturnStmt rStmt)) $+$ rbrace 

instance PrettyPrinter ModalityDecl where
    pPrt ModalityD_val = text "val" 
    pPrt ModalityD_var = text "var"

instance PrettyPrinter ModalityParam where
    pPrt ModalityPEmpty = empty
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

instance PrettyPrinter Double where
    pPrt double = text (show double) 

instance PrettyPrinter Boolean where
    pPrt Boolean_True = text "true"
    pPrt Boolean_False = text "false"

instance PrettyPrinter BasicType where
    pPrt (BType tp) = text tp

instance PrettyPrinter TypeSpec where
    pPrt (BasTyp basType) = pPrt (gBasicType basType)
    pPrt (CompType compType) = pPrt (gCompoundType compType)

instance PrettyPrinter CompoundType where
    pPrt (ArrDef tpSpec int) = text "Array" <> (brackets (pPrt (gTypeSpec tpSpec))) <> (parens (pPrt int))
    pPrt (Pointer tpSpec) = text "*" <> pPrt (gTypeSpec tpSpec)

instance PrettyPrinter ComplexRExpr where
    pPrt (Simple rExp) = pPrt (gRExpr rExp)
    pPrt (Array cRexprs) = text "Array" <> (parens (hcat (punctuate comma (map pPrt (map gComplexRExpr cRexprs)))))

instance PrettyPrinter Parameter where
    pPrt (Param modP ident tpSpec) = pPrt (gModalityParam modP) <+> pPrt ident <> colon <> pPrt (gTypeSpec tpSpec)

instance PrettyPrinter RExpr where
    pPrt (OpRelation rExp1 rExp2 op) = pPrt (gRExpr rExp1) <+> text op <+> pPrt(gRExpr rExp2)
    pPrt (OpAritm rExp1 rExp2 op) = pPrt (gRExpr rExp1) <+> text op <+> pPrt(gRExpr rExp2)
    pPrt (OpBoolean rExp1 rExp2 op) = pPrt (gRExpr rExp1) <+> text op <+> pPrt(gRExpr rExp2)
    pPrt (Not rExp) = text "!" <> pPrt (gRExpr rExp) 
    pPrt (Neg rExp) = text "-" <> pPrt (gRExpr rExp)
    pPrt (Ref lExp) = text "&" <> pPrt (gLExpr lExp)
    pPrt (FCall fcall) = pPrt (gFunCall fcall)
    pPrt (Int int) = pPrt int
    pPrt (Char char) = pPrt char
    pPrt (String string) = pPrt string
    pPrt (Float float) = pPrt float
    pPrt (Bool bool) = pPrt bool
    pPrt (Lexpr lExp) = pPrt (gLExpr lExp)

instance PrettyPrinter LExpr where
    pPrt (Deref rExp) = text "*" <> pPrt (gRExpr rExp)
    pPrt (PreIncrDecr lExp op) = text op <> text op <> pPrt (gLExpr lExp)
    pPrt (PostIncrDecr lExp op) = pPrt (gLExpr lExp) <> text op <> text op
    pPrt (BasLExpr bLExp) = pPrt (gBLExpr bLExp)

instance PrettyPrinter FunCall where
    pPrt (Call ident rExprs) = pPrt ident <> (parens (hcat (punctuate comma (map pPrt (map gRExpr rExprs))))) 

instance PrettyPrinter BLExpr where
    pPrt (ArrayEl bLExp rExp) = pPrt (gBLExpr bLExp) <> (brackets (pPrt (gRExpr rExp)))
    pPrt (Id ident) = pPrt ident 

instance PrettyPrinter CompStmt where
    pPrt (BlockDecl decls stmts) = (vcat (map pPrt (map gDecl decls))) $$ (vcat (map pPrt (map gStmt stmts)))

instance PrettyPrinter Stmt where
    pPrt (Comp cStmt) = pPrt (gCompStmt cStmt) 
    pPrt (ProcCall fcall) = pPrt (gFunCall fcall) <> semi
    pPrt (Jmp jStmt) = pPrt (gJumpStmt jStmt) <> semi
    pPrt (Iter iStmt) = pPrt (gIterStmt iStmt) 
    pPrt (Sel sStmt) = pPrt (gSelectionStmt sStmt)
    pPrt (Assgn lExp op rExp) = pPrt (gLExpr lExp) <+> pPrt (gAssignment_op op) <+> pPrt (gRExpr rExp) <> semi
    pPrt (LExprStmt lExp) = pPrt (gLExpr lExp) <> semi 
    pPrt (ExHandler tcStmt) = pPrt (gTryCatch tcStmt)

instance PrettyPrinter TryCatchStmt where
    pPrt (TryCatch stm1 ident stm2) = text "try" <+> lbrace $+$ nest tab (pPrt (gStmt stm1)) $+$ rbrace <+> text "catch" <+> lbrace $$ nest tab (text "case" <+> text "ex" <> colon <+> pPrt ident <+> text "=>" <+> lbrace $+$ nest tab (pPrt (gStmt stm2)) $+$ rbrace) $+$ rbrace

instance PrettyPrinter Assignment_op where
    pPrt Assign = equals
    pPrt (AssignOp op) = text op <> equals

instance PrettyPrinter JumpStmt where
    pPrt Break = text "break"
    pPrt Continue = text "continue"

instance PrettyPrinter ReturnStmt where
    pPrt RetExpVoid = text "return" <> semi
    pPrt (RetExp rExp) = text "return" <> (parens (pPrt (gRExpr rExp))) <> semi

instance PrettyPrinter SelectionStmt where
    pPrt (IfNoElse rExp stm) = text "if" <> (parens (pPrt (gRExpr rExp))) <+> lbrace $+$ nest tab (pPrt (gStmt stm)) $+$ rbrace  
    pPrt (IfElse rExp stm1 stm2) = text "if" <> (parens (pPrt (gRExpr rExp))) <+> lbrace $+$ nest tab (pPrt (gStmt stm1)) $+$ rbrace <+> text "else" <+> lbrace $+$ nest tab (pPrt (gStmt stm2)) $+$ rbrace 

instance PrettyPrinter IterStmt where
    pPrt (While rExp stm) = text "while" <> (parens (pPrt (gRExpr rExp))) <+> lbrace $+$ nest tab (pPrt (gStmt stm)) $+$ rbrace 
    pPrt (For ident rExp1 rExp2 stm) = text "for" <> (parens ((pPrt ident) <+> text "<-" <+> (pPrt (gRExpr rExp1)) <+> text "to" <+> (pPrt (gRExpr rExp2)))) <+> lbrace $+$ nest tab (pPrt (gStmt stm)) $+$ rbrace

class PrettyPrinter a where
  pPrt :: a -> Doc
