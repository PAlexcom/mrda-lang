{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module PrettyPrinterABS where

import Parser

import Text.PrettyPrint hiding (Str)

tab = 1

instance PrettyPrinter Program where
    pPrt (Prog decls) = vcat ( map pPrt (map decl decls))

instance PrettyPrinter Decl where
    pPrt (DvarBInit modD ident basType cRExpr) =
        pPrt (modalityDecl modD) <+> pPrt ident <> colon <+> pPrt (basicType basType) <+> text "=" <+> pPrt (complexRExpr cRExpr) <> semi
    pPrt (DvarCInit modD ident tpSpec cRExpr) =
        pPrt (modalityDecl modD) <+> pPrt ident <> colon <+> pPrt (typeSpec tpSpec) <+> text "=" <+> pPrt (complexRExpr cRExpr) <> semi
    pPrt (Dfun ident params basType cStmt rStmt) =
        text "def" <+> pPrt ident <+> (parens (hsep (punctuate comma (map pPrt (map parameter params))))) <> colon <> pPrt (basicType basType) <+> text "=" <+> lbrace $$ nest tab ((pPrt (compStmt cStmt) $+$ pPrt (returnStmt rStmt) $+$ rbrace ))

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
    pPrt (BasTyp basType) = pPrt (basicType basType)
    pPrt (CompType compType) = pPrt (compoundType compType)

instance PrettyPrinter CompoundType where
    pPrt (ArrDef tpSpec int) = text "Array" <> (brackets (pPrt (typeSpec tpSpec))) <> (parens (pPrt int))
    pPrt (Pointer tpSpec) = text "*" <> pPrt (typeSpec tpSpec)

instance PrettyPrinter ComplexRExpr where
    pPrt (Simple rExp) = pPrt (rExpr rExp)
    pPrt (Array cRexprs) = text "Array" <> (parens (hcat (punctuate comma (map pPrt (map complexRExpr cRexprs)))))

instance PrettyPrinter Parameter where
    pPrt (Param modP ident tpSpec) = pPrt (modalityParam modP) <+> pPrt ident <> colon <> pPrt (typeSpec tpSpec)

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
    pPrt (Call ident rExprs) = pPrt ident <> (parens (hcat (punctuate comma (map pPrt (map rExpr rExprs))))) 

instance PrettyPrinter BLExpr where
    pPrt (ArrayEl bLExp rExp) = pPrt (bLExpr bLExp) <> (brackets (pPrt (rExpr rExp)))
    pPrt (Id ident) = pPrt ident 

instance PrettyPrinter CompStmt where
    pPrt (BlockDecl decls stmts) = (vcat (map pPrt (map decl decls))) $$ (vcat (map pPrt (map stmt stmts)))

instance PrettyPrinter Stmt where
    pPrt (Comp cStmt) = lbrace $+$ nest tab (pPrt (compStmt cStmt) $+$ rbrace ) 
    pPrt (ProcCall fcall) = pPrt (funCall fcall) <> semi
    pPrt (Jmp jStmt) = pPrt (jumpStmt jStmt) <> semi
    pPrt (Iter iStmt) = pPrt (iterStmt iStmt) 
    pPrt (Sel sStmt) = pPrt (selectionStmt sStmt)
    pPrt (Assgn lExp op rExp) = pPrt (lExpr lExp) <+> pPrt (assignment_op op) <+> pPrt (rExpr rExp) <> semi
    pPrt (LExprStmt lExp) = pPrt (lExpr lExp) <> semi 
    pPrt (ExHandler tcStmt) = pPrt (tryCatch tcStmt)

instance PrettyPrinter TryCatchStmt where
    pPrt (TryCatch stm1 ident stm2) = text "try" <+> pPrt (stmt stm1) $+$ text "catch" <+> lbrace $$ nest tab (text "case" <+> text "ex" <> colon <+> pPrt ident <+> text "=>" <+> pPrt (stmt stm2) $+$ rbrace)

instance PrettyPrinter Assignment_op where
    pPrt Assign = equals
    pPrt (AssignOp op) = text op <> equals

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
    pPrt (For ident rExp1 rExp2 stm) = text "for" <> (parens ((pPrt ident) <+> text "<-" <+> (pPrt (rExpr rExp1)) <+> text "to" <+> (pPrt (rExpr rExp2)))) <+> pPrt (stmt stm)

class PrettyPrinter a where
  pPrt :: a -> Doc
