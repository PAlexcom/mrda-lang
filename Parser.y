{
module Parser where

import Lexer
import Error
}

%name pProgram Program

%monad { Err } { thenM } { returnM }
%tokentype {Token}

%token
    ':'         { Token _ (TokenSymbols ":") }
    '!'         { Token _ (TokenSymbols "!") }
    '!='        { Token _ (TokenSymbols "!=") }
    '%'         { Token _ (TokenSymbols "%") }
    '&'         { Token _ (TokenSymbols "&") }
    '&&'        { Token _ (TokenSymbols "&&") }
    '('         { Token _ (TokenSymbols "(") }
    ')'         { Token _ (TokenSymbols ")") }
    '*'         { Token _ (TokenSymbols "*") }
    '*='        { Token _ (TokenSymbols "*=") }
    '+'         { Token _ (TokenSymbols "+") }
    '++'        { Token _ (TokenSymbols "++") }
    '+='        { Token _ (TokenSymbols "+=") }
    ','         { Token _ (TokenSymbols ",") }
    '-'         { Token _ (TokenSymbols "-") }
    '--'        { Token _ (TokenSymbols "--") }
    '-='        { Token _ (TokenSymbols "-=") }
    '/'         { Token _ (TokenSymbols "/") }
    '/='        { Token _ (TokenSymbols "/=") }
    ';'         { Token _ (TokenSymbols ";") }
    '<'         { Token _ (TokenSymbols "<") }
    '<='        { Token _ (TokenSymbols "<=") }
    '<-'        { Token _ (TokenSymbols "<-") }
    '='         { Token _ (TokenSymbols "=") }
    '=='        { Token _ (TokenSymbols "==") }
    '>'         { Token _ (TokenSymbols ">") }
    '>='        { Token _ (TokenSymbols ">=") }
    '=>'        { Token _ (TokenSymbols "=>") }
    'to'        { Token _ (TokenSymbols "to") }
    'ex'        { Token _ (TokenSymbols "ex") }
    'case'      { Token _ (TokenSymbols "case") }
    'try'       { Token _ (TokenSymbols "try") }
    'catch'     { Token _ (TokenSymbols "catch") }
    'false'     { Token _ (TokenSymbols "false") }
    'true'      { Token _ (TokenSymbols "true") }
    '['         { Token _ (TokenSymbols "[") }
    ']'         { Token _ (TokenSymbols "]") }
    '^'         { Token _ (TokenSymbols "^") }
    'bool'      { Token _ (TokenSymbols "Boolean") }
    'break'     { Token _ (TokenSymbols "break") }
    'char'      { Token _ (TokenSymbols "Char") }
    'continue'  { Token _ (TokenSymbols "continue") }
    'do'        { Token _ (TokenSymbols "do") }
    'else'      { Token _ (TokenSymbols "else") }
    'float'     { Token _ (TokenSymbols "Float") }
    'if'        { Token _ (TokenSymbols "if") }
    'int'       { Token _ (TokenSymbols "Int") }
    'string'    { Token _ (TokenSymbols "String") }
    'array'     { Token _ (TokenSymbols "Array") }
    'return'    { Token _ (TokenSymbols "return") }
    'val'       { Token _ (TokenSymbols "val") }
    'var'       { Token _ (TokenSymbols "var") }
    'valres'    { Token _ (TokenSymbols "valres") }
    'unit'      { Token _ (TokenSymbols "Unit") }
    'while'     { Token _ (TokenSymbols "while") }
    'for'       { Token _ (TokenSymbols "for") }
    'def'       { Token _ (TokenSymbols "def") }
    '{'         { Token _ (TokenSymbols "{") }
    '}'         { Token _ (TokenSymbols "}") }
    '||'        { Token _ (TokenSymbols "||") }
    Int         { Token _ (TokenInt _) }
    Char        { Token _ (TokenChar _) }
    String      { Token _ (TokenAlpha _) }
    Double      { Token _ (TokenDouble _) }
    Ident       { Token _ (TokenIdent _) }

%left '||' '&&'
%left '!'
%nonassoc '<' '>' '<=' '<-' '>=' '=>' '==' '!='
%left '+' '-'
%left '*' '/' '%'
%right '^'
%left NEG '&'
%left '++' '--'
%%

BasicType :: {AbsNode}
    : 'bool'                        { BasicTypeNode (tpos $1) (BType "Boolean") }
    | 'char'                        { BasicTypeNode (tpos $1) (BType "Char") }
    | 'float'                       { BasicTypeNode (tpos $1) (BType "Float") }
    | 'int'                         { BasicTypeNode (tpos $1) (BType "Int") }
    | 'unit'                        { BasicTypeNode (tpos $1) (BType "Unit") }
    | 'string'                      { BasicTypeNode (tpos $1) (BType "String") }

RExpr :: {AbsNode} 
    : RExpr '||' RExpr              { RExprNode (pos $1) (OpBoolean $1 $3 "||") }
    | RExpr '&&' RExpr              { RExprNode (pos $1) (OpBoolean $1 $3 "&&") }
    | '!' RExpr                     { RExprNode (tpos $1) (Not $2) }
    | RExpr '==' RExpr              { RExprNode (pos $1) (OpRelation $1 $3 "==") }
    | RExpr '!=' RExpr              { RExprNode (pos $1) (OpRelation $1 $3 "!=") }
    | RExpr '<' RExpr               { RExprNode (pos $1) (OpRelation $1 $3 "<") }
    | RExpr '<=' RExpr              { RExprNode (pos $1) (OpRelation $1 $3 "<=") }
    | RExpr '>' RExpr               { RExprNode (pos $1) (OpRelation $1 $3 ">") }
    | RExpr '>=' RExpr              { RExprNode (pos $1) (OpRelation $1 $3 ">=") }
    | RExpr '+' RExpr               { RExprNode (pos $1) (OpAritm $1 $3 "+") }
    | RExpr '-' RExpr               { RExprNode (pos $1) (OpAritm $1 $3 "-") }
    | RExpr '*' RExpr               { RExprNode (pos $1) (OpAritm $1 $3 "*") }
    | RExpr '/' RExpr               { RExprNode (pos $1) (OpAritm $1 $3 "/") }   
    | RExpr '%' RExpr               { RExprNode (pos $1) (OpAritm $1 $3 "%") }
    | RExpr '^' RExpr               { RExprNode (pos $1) (OpAritm $1 $3 "^") }
    | '-' RExpr %prec NEG           { RExprNode (tpos $1) (Neg $2) }
    | '&' LExpr                     { RExprNode (tpos $1) (Ref $2) }
    | FunCall                       { RExprNode (pos $1) (FCall $1) }
    | Int                           { RExprNode (tpos $1) (Int (read (prToken $1)::Int)) }
    | Char                          { RExprNode (tpos $1) (Char (head (tail(prToken $1)))) }
    | String                        { RExprNode (tpos $1) (String (prToken $1)) }
    | Double                        { RExprNode (tpos $1) (Float (read (prToken $1)::Double)) }
    | 'true'                        { RExprNode (tpos $1) (Bool Boolean_True) }
    | 'false'                       { RExprNode (tpos $1) (Bool Boolean_False) }
    | '(' RExpr ')'                 { $2 }
    | LExpr                         { RExprNode (pos $1) (Lexpr $1) }

LExpr :: {AbsNode} 
    : '*' RExpr                     { LExprNode (tpos $1) (Deref $2) }
    | '++' LExpr                    { LExprNode (tpos $1) (PreIncrDecr $2 "+") }
    | '--' LExpr                    { LExprNode (tpos $1) (PreIncrDecr $2 "-") }
    | LExpr '++'                    { LExprNode (pos $1) (PostIncrDecr $1 "+") }
    -- TODO sistemare il post incremento per l-value
    | LExpr '--'                    { LExprNode (pos $1) (PostIncrDecr $1 "-") }
    | '(' LExpr ')'                 { $2 }
    | BLExpr                        { LExprNode (pos $1) (BasLExpr $1) }

FunCall :: {AbsNode} 
    : Ident '(' ListRExpr ')'       { FunCallNode (tpos $1) (Call (Ident (prToken $1)) $3) }

ListRExpr :: {[AbsNode]} 
    : {- empty -}                   { [] }
    | RExpr                         { (:[]) $1 }
    | RExpr ',' ListRExpr           { (:) $1 $3 }

BLExpr :: {AbsNode} 
    : BLExpr '[' RExpr ']'          { BLExprNode (pos $1) (ArrayEl $1 $3) }
    | Ident                         { BLExprNode (tpos $1) (Id (Ident (prToken $1))) }

Program :: {AbsNode} 
    : ListDecl                      { ProgramNode (Pn 1 1) (Prog (reverse $1)) }

ListDecl :: {[AbsNode]}  
    : {- empty -}                   { [] }
    | ListDecl Decl                 { flip (:) $1 $2 }

Decl :: {AbsNode} 
    : ModalityDecl Ident ':' BasicType '=' ComplexRExpr ';'                             { DeclNode (pos $1) (DvarBInit $1 (Ident (prToken $2)) $4 $6) }
    | ModalityDecl Ident ':' TypeSpec '=' ComplexRExpr ';'                              { DeclNode (pos $1) (DvarCInit $1 (Ident (prToken $2)) $4 $6) }
    | 'def' Ident '(' ListParameter ')' ':' BasicType '=' '{' CompStmt ReturnStmt '}'   { DeclNode (tpos $1) (Dfun (Ident (prToken $2)) $4 $7 $10 $11) }

TypeSpec :: {AbsNode} 
    : BasicType                             { TypeSpecNode (pos $1) (BasTyp $1) }
    | CompoundType                          { TypeSpecNode (pos $1) (CompType $1) }

CompoundType :: {AbsNode} 
    : 'array' '[' TypeSpec ']' '(' Int ')'          { CompoundTypeNode (tpos $1) (ArrDef $3 (Just (read (prToken $6)::Int))) }
    | 'array' '[' TypeSpec ']'                      { CompoundTypeNode (tpos $1) (ArrDef $3 Nothing) }
    | '*' TypeSpec                                  { CompoundTypeNode (tpos $1) (Pointer $2) }

ComplexRExpr :: {AbsNode} 
    : RExpr                                 { ComplexRExprNode (pos $1) (Simple $1) }
    | 'array' '(' ListComplexRExpr ')'      { ComplexRExprNode (tpos $1) (Array $3) }

ListComplexRExpr : ComplexRExpr             { (:[]) $1 }
    | ComplexRExpr ',' ListComplexRExpr     { (:) $1 $3 }

ListParameter :: {[AbsNode]}  
    : {- empty -}                           { [] }
    | Parameter                             { (:[]) $1 }
    | Parameter ',' ListParameter           { (:) $1 $3 }

Parameter :: {AbsNode}
    : ModalityParam Ident ':' TypeSpec      { ParameterNode (pos $1) (Param $1 (Ident (prToken $2)) $4) }

ModalityDecl :: {AbsNode}
    : 'val'                                 { ModalityDeclNode (tpos $1) ModalityD_val }
    | 'var'                                 { ModalityDeclNode (tpos $1) ModalityD_var }

ModalityParam :: {AbsNode} 
    : {- empty -}                           { ModalityParamNode (Pn 0 0) ModalityPEmpty }
    | 'val'                                 { ModalityParamNode (tpos $1) ModalityP_val }
    | 'valres'                              { ModalityParamNode (tpos $1) ModalityP_valres }
    | 'var'                                 { ModalityParamNode (tpos $1) ModalityP_var }

CompStmt :: {AbsNode}                       -- FIXME: previous declaration: pos (head $1), returns error because if the list is empty it throws an error, you should write a function 'head' that handles empty lists
    : ListDecl ListStmt                     { CompStmtNode (Pn 0 0) (BlockDecl (reverse $1) (reverse $2)) }

ListStmt
    : {- empty -}                           { [] }
    | ListStmt Stmt                         { flip (:) $1 $2 }

Stmt :: {AbsNode}
    : '{' CompStmt '}'                     { StmtNode (tpos $1) (Comp $2) }
    | FunCall ';'                          { StmtNode (pos $1) (ProcCall $1) }
    | JumpStmt ';'                         { StmtNode (pos $1) (Jmp $1) }
    | IterStmt                             { StmtNode (pos $1) (Iter $1) }
    | SelectionStmt                        { StmtNode (pos $1) (Sel $1) }
    | LExpr Assignment_op RExpr ';'        { StmtNode (pos $1) (Assgn $1 $2 $3) }
    | LExpr ';'                            { StmtNode (pos $1) (LExprStmt $1) }
    | TryCatchStmt                         { StmtNode (pos $1) (ExHandler $1)}

TryCatchStmt :: {AbsNode}
    : 'try' Stmt 'catch' '{' 'case' 'ex' ':' Ident '=>' Stmt '}'    { TryCatchStmtNode (tpos $1) (TryCatch $2 (Ident (prToken $8)) $10) }

Assignment_op :: {AbsNode} 
    : '='                                  { Assignment_opNode (tpos $1) Assign }
    | '*='                                 { Assignment_opNode (tpos $1) (AssignOp "*") }
    | '+='                                 { Assignment_opNode (tpos $1) (AssignOp "+") }
    | '/='                                 { Assignment_opNode (tpos $1) (AssignOp "/") }
    | '-='                                 { Assignment_opNode (tpos $1) (AssignOp "-") }

JumpStmt :: {AbsNode} 
    : 'break'                              { JumpStmtNode (tpos $1) Break }
    | 'continue'                           { JumpStmtNode (tpos $1) Continue }

ReturnStmt :: {AbsNode} 
    : 'return' ';'                         { ReturnStmtNode (tpos $1) RetExpVoid }
    | 'return' '(' RExpr ')' ';'           { ReturnStmtNode (tpos $1) (RetExp $3) }
 

SelectionStmt :: {AbsNode} 
    : 'if' '(' RExpr ')' Stmt 'else' Stmt  { SelectionStmtNode (tpos $1) (IfElse $3 $5 $7) }
    | 'if' '(' RExpr ')' Stmt              { SelectionStmtNode (tpos $1) (IfNoElse $3 $5) }

IterStmt :: {AbsNode} 
    : 'while' '(' RExpr ')' Stmt                        { IterStmtNode (tpos $1) (While $3 $5) }
    | 'do' Stmt 'while' '(' RExpr ')' ';'               { IterStmtNode (tpos $1) (DoWhile $2 $5) }
    | 'for' '(' Ident '<-' RExpr 'to' RExpr ')' Stmt    { IterStmtNode (tpos $1) (For (Ident (prToken $3)) $5 $7 $9) }

{

data AbsNode 
    = RExprNode         {pos::Posn, rExpr::RExpr}
    | FunCallNode       {pos::Posn, funCall::FunCall}
    | LExprNode         {pos::Posn, lExpr::LExpr}
    | BLExprNode        {pos::Posn, bLExpr::BLExpr}
    | ProgramNode       {pos::Posn, program::Program}
    | DeclNode          {pos::Posn, decl::Decl}
    | TypeSpecNode      {pos::Posn, typeSpec::TypeSpec}
    | BasicTypeNode     {pos::Posn, basicType::BasicType}
    | CompoundTypeNode  {pos::Posn, compoundType::CompoundType}
    | ComplexRExprNode  {pos::Posn, complexRExpr::ComplexRExpr}
    | ParameterNode     {pos::Posn, parameter::Parameter}
    | ModalityParamNode {pos::Posn, modalityParam::ModalityParam}
    | ModalityDeclNode  {pos::Posn, modalityDecl::ModalityDecl}
    | CompStmtNode      {pos::Posn, compStmt::CompStmt}
    | StmtNode          {pos::Posn, stmt::Stmt}
    | TryCatchStmtNode  {pos::Posn, tryCatch::TryCatchStmt}
    | Assignment_opNode {pos::Posn, assignment_op::Assignment_op}
    | JumpStmtNode      {pos::Posn, jumpStmt::JumpStmt} 
    | ReturnStmtNode    {pos::Posn, returnStmt::ReturnStmt}
    | SelectionStmtNode {pos::Posn, selectionStmt::SelectionStmt}
    | IterStmtNode      {pos::Posn, iterStmt::IterStmt}
    deriving (Eq, Ord, Show)

newtype Ident = Ident String
    deriving (Eq, Ord, Show, Read)

data Boolean = Boolean_True | Boolean_False
    deriving (Eq, Ord, Show, Read)

data Program = Prog [AbsNode]
    deriving (Eq, Ord, Show)

data Decl
    = DvarBInit AbsNode Ident AbsNode AbsNode
    | DvarCInit AbsNode Ident AbsNode AbsNode
    | Dfun Ident [AbsNode] AbsNode AbsNode AbsNode
    deriving (Eq, Ord, Show)

data RExpr
    = OpRelation AbsNode AbsNode String
    | OpAritm AbsNode AbsNode String
    | OpBoolean AbsNode AbsNode String
    | Not AbsNode
    | Neg AbsNode
    | Ref AbsNode
    | FCall AbsNode
    | Int Int
    | Char Char
    | String String
    | Float Double
    | Bool Boolean
    | Lexpr AbsNode
    deriving (Eq, Ord, Show)

data FunCall
    = Call Ident [AbsNode]
    deriving (Eq, Ord, Show)

data LExpr
    = Deref AbsNode
    | PreIncrDecr AbsNode String
    | PostIncrDecr AbsNode String
    | BasLExpr AbsNode
    deriving (Eq, Ord, Show)

data BLExpr
    = ArrayEl AbsNode AbsNode
    | Id Ident
    deriving (Eq, Ord, Show)

data TypeSpec
    = BasTyp AbsNode
    | CompType AbsNode
    deriving (Eq, Ord, Show)

data BasicType
    = BType String
    deriving (Eq, Ord, Show, Read)

data CompoundType
    = ArrDef AbsNode (Maybe Int)
    | Pointer AbsNode
    deriving (Eq, Ord, Show)

data ComplexRExpr
    = Simple AbsNode
    | Array [AbsNode]
    deriving (Eq, Ord, Show)

data Parameter
    = Param AbsNode Ident AbsNode
    deriving (Eq, Ord, Show)

data ModalityParam
    = ModalityPEmpty
    | ModalityP_val
    | ModalityP_var
    | ModalityP_valres
    deriving (Eq, Ord, Show)

data ModalityDecl
    = ModalityD_val
    | ModalityD_var
    deriving (Eq, Ord, Show)

data CompStmt
    = BlockDecl [AbsNode] [AbsNode]
    deriving (Eq, Ord, Show)

data Stmt
    = Comp AbsNode
    | ProcCall AbsNode
    | Jmp AbsNode
    | Iter AbsNode
    | Sel AbsNode
    | Assgn AbsNode AbsNode AbsNode
    | LExprStmt AbsNode
    | ExHandler AbsNode
    deriving (Eq, Ord, Show)

data TryCatchStmt
    = TryCatch AbsNode Ident AbsNode
    deriving (Eq, Ord, Show)

data Assignment_op 
    = Assign
    | AssignOp String
    deriving (Eq, Ord, Show, Read)

data JumpStmt 
    = Break
    | Continue
    deriving (Eq, Ord, Show, Read)

data ReturnStmt 
    = RetExpVoid
    | RetExp AbsNode
    deriving (Eq, Ord, Show) 

data SelectionStmt
    = IfNoElse AbsNode AbsNode
    | IfElse AbsNode AbsNode AbsNode
    deriving (Eq, Ord, Show)

data IterStmt 
    = While AbsNode AbsNode 
    | DoWhile AbsNode AbsNode
    | For Ident AbsNode AbsNode AbsNode
    deriving (Eq, Ord, Show)


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "Error => syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))
}
