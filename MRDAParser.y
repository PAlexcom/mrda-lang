{
module MRDAParser where

import MRDALexer
import Error
}

%name pProgram Program
%name pStmt Stmt
%name pRExpr RExpr
%name pLExpr LExpr

%monad { Err } { thenM } { returnM }
%tokentype {Token}

%token
    '!'         { Token _ (TokenSymbols "!") }
    '!='        { Token _ (TokenSymbols "!=") }
    '%'         { Token _ (TokenSymbols "%") }
    '&'         { Token _ (TokenSymbols "&") }
    '&&'        { Token _ (TokenSymbols "&&") }
    '('         { Token _ (TokenSymbols "(") }
    ')'         { Token _ (TokenSymbols ")") }
  '*'           { Token _ (TokenSymbols "*") }
  '*='          { Token _ (TokenSymbols "*=") }
  '+'           { Token _ (TokenSymbols "+") }
  '++'          { Token _ (TokenSymbols "++") }
  '+='          { Token _ (TokenSymbols "+=") }
  ','           { Token _ (TokenSymbols ",") }
  '-'           { Token _ (TokenSymbols "-") }
  '--'          { Token _ (TokenSymbols "--") }
  '-='          { Token _ (TokenSymbols "-=") }
  '/'           { Token _ (TokenSymbols "/") }
  '/='          { Token _ (TokenSymbols "/=") }
  ';'           { Token _ (TokenSymbols ";") }
  '<'           { Token _ (TokenSymbols "<") }
  '<='          { Token _ (TokenSymbols "<=") }
  '='           { Token _ (TokenSymbols "=") }
  '=='          { Token _ (TokenSymbols "==") }
  '>'           { Token _ (TokenSymbols ">") }
  '>='          { Token _ (TokenSymbols ">=") }
  'False'       { Token _ (TokenSymbols "False") }
  'True'        { Token _ (TokenSymbols "True") }
  '['           { Token _ (TokenSymbols "[") }
  ']'           { Token _ (TokenSymbols "]") }
  '^'           { Token _ (TokenSymbols "^") }
  'bool'        { Token _ (TokenSymbols "bool") }
  'break'       { Token _ (TokenSymbols "break") }
  'char'        { Token _ (TokenSymbols "char") }
  'const'       { Token _ (TokenSymbols "const") }
  'continue'    { Token _ (TokenSymbols "continue") }
  'do'          { Token _ (TokenSymbols "do") }
  'else'        { Token _ (TokenSymbols "else") }
  'float'       { Token _ (TokenSymbols "float") }
  'if'          { Token _ (TokenSymbols "if") }
  'int'         { Token _ (TokenSymbols "int") }
  'name'        { Token _ (TokenSymbols "name") }
  'ref'         { Token _ (TokenSymbols "ref") }
  'res'         { Token _ (TokenSymbols "res") }
  'return'      { Token _ (TokenSymbols "return") }
  'val'         { Token _ (TokenSymbols "val") }
  'valres'      { Token _ (TokenSymbols "valres") }
  'void'        { Token _ (TokenSymbols "void") }
  'while'       { Token _ (TokenSymbols "while") }
  '{'           { Token _ (TokenSymbols "{") }
  '}'           { Token _ (TokenSymbols "}") }
  '||'          { Token _ (TokenSymbols "||") }
    L_integ     { Token _ (TokenInt $$) }
    L_charac    { Token _ (TokenChar $$) }
    L_quoted    { Token _ (TokenAlpha $$) }
    L_doubl     { Token _ (TokenDouble $$) }
    L_ident     { Token _ (TokenIdent $$) }

%left '||' '&&'
%left '!'
%nonassoc '<' '>' '<=' '>=' '==' '!='
%left '+' '-'
%left '*' '/' '%'
%right '^'
%left NEG '&'
%left '++' '--'
%%

Integer :: { Integer } : L_integ  { (read ( $1)) :: Integer }
Char    :: { Char }    : L_charac { (read ( $1)) :: Char }
String  :: { String }  : L_quoted {  $1 }
Double  :: { Double }  : L_doubl  { (read ( $1)) :: Double }
Ident   :: { Ident }   : L_ident  { Ident $1 }

Boolean : 'True'            { Boolean_True }
        | 'False'           { Boolean_False }

RExpr : RExpr '||' RExpr   { Or $1 $3 }
    | RExpr '&&' RExpr    { And $1 $3 }
    | '!' RExpr            { Not $2 }
    | RExpr '==' RExpr    { Eq $1 $3 }
    | RExpr '!=' RExpr    { Neq $1 $3 }
    | RExpr '<' RExpr     { Lt $1 $3 }
    | RExpr '<=' RExpr    { LtE $1 $3 }
    | RExpr '>' RExpr     { Gt $1 $3 }
    | RExpr '>=' RExpr    { GtE $1 $3 }
    | RExpr '+' RExpr     { Add $1 $3 }
    | RExpr '-' RExpr     { Sub $1 $3 }
    | RExpr '*' RExpr     { Mul $1 $3 }
    | RExpr '/' RExpr     { Div $1 $3 }
    | RExpr '%' RExpr     { Mod $1 $3 }
    | RExpr '^' RExpr    { Pow $1 $3 }
    | '-' RExpr %prec NEG           { Neg $2 }
    | '&' LExpr             { Ref $2 }
    | FunCall               { FCall $1 }
    | Integer               { Int $1 }
    | Char                  { Char $1 }
    | String                { String $1 }
    | Double                { Float $1 }
    | Boolean               { Bool $1 }
    |'(' RExpr ')'          { $2 }
    | LExpr                 { Lexpr $1 }

FunCall : Ident '(' ListRExpr ')'   { Call $1 $3 }

ListRExpr : {- empty -}             { [] }
          | RExpr                   { (:[]) $1 }
          | RExpr ',' ListRExpr     { (:) $1 $3 }

LExpr : '*' RExpr                   { Deref $2 }
    | '++' LExpr                    { PreInc $2 }
    | '--' LExpr                    { PreDecr $2 }
    | LExpr '++'                    { PostInc $1 }
    | LExpr '--'                    { PostDecr $1 }
    | '(' LExpr ')'                 { $2 }
    | BLExpr                        { BasLExpr $1 }

BLExpr : BLExpr '[' RExpr ']'       { ArrayEl $1 $3 }
    | Ident                         { Id $1 }

Program : ListDecl                  { Prog (reverse $1) }

ListDecl : {- empty -}              { [] }
    | ListDecl Decl                 { flip (:) $1 $2 }

Decl : BasicType ListVarDeclInit ';'                    { DvarBInit $1 $2 }
    | TypeSpec ListVarDeclInit ';'                      { DvarCInit $1 $2 }
    | BasicType Ident '(' ListParameter ')' CompStmt    { Dfun $1 $2 $4 $6 }

ListVarDeclInit : VarDeclInit                           { (:[]) $1 }
    | VarDeclInit ',' ListVarDeclInit                   { (:) $1 $3 }

TypeSpec : BasicType                { BasTyp $1 }
    | CompoundType                  { CompType $1 }

BasicType : 'bool'                  { BasicType_bool }
    | 'char'                        { BasicType_char }
    | 'float'                       { BasicType_float }
    | 'int'                         { BasicType_int }
    | 'void'                        { BasicType_void }

CompoundType : TypeSpec '[' Integer ']'                 { ArrDef $1 $3 }
    | TypeSpec '[' ']'                                  { ArrUnDef $1 }
    | TypeSpec '*'                                      { Pointer $1 }

VarDeclInit : Ident '=' ComplexRExpr                    { VarDeclIn $1 $3 }

ComplexRExpr : RExpr                                    { Simple $1 }
    | '[' ListComplexRExpr ']'                          { Array $2 }

ListComplexRExpr : ComplexRExpr                         { (:[]) $1 }
    | ComplexRExpr ',' ListComplexRExpr                 { (:) $1 $3 }

ListParameter : {- empty -}                             { [] }
    | Parameter                                         { (:[]) $1 }
    | Parameter ',' ListParameter                       { (:) $1 $3 }

Parameter : Modality TypeSpec Ident                     { Param $1 $2 $3 }

Modality : {- empty -} { ModalityEmpty }
         | 'val' { Modality_val }
         | 'ref' { Modality_ref }
         | 'const' { Modality_const }
         | 'res' { Modality_res }
         | 'valres'                                     { Modality_valres }
         | 'name'                                       { Modality_name }

CompStmt : CompStmtNocolon ';'                          { CompStmt1 $1 }
         | CompStmtNocolon                              { CompStmtCompStmtNocolon $1 }

CompStmtNocolon : '{' ListDecl ListStmt '}'             { BlockDecl (reverse $2) (reverse $3) }

ListStmt : {- empty -}                                  { [] }
    | ListStmt Stmt                                     { flip (:) $1 $2 }

Stmt : CompStmt                                         { Comp $1 }
     | FunCall ';'                                      { ProcCall $1 }
     | JumpStmt ';'                                     { Jmp $1 }
     | IterStmt                                         { Iter $1 }
     | SelectionStmt { Sel $1 }
     | LExpr Assignment_op RExpr ';' { Assgn $1 $2 $3 }
     | LExpr ';' { LExprStmt $1 }

Assignment_op : '=' { Assign }
              | '*=' { AssgnMul }
              | '+=' { AssgnAdd }
              | '/=' { AssgnDiv }
              | '-=' { AssgnSub }

JumpStmt : 'break' { Break }
         | 'continue' { Continue }
         | 'return' { RetExpVoid }
         | 'return' '(' RExpr ')' { RetExp $3 }

SelectionStmt : 'if' '(' RExpr ')' Stmt 'else' Stmt { IfElse $3 $5 $7 }
              | 'if' '(' RExpr ')' Stmt { IfNoElse $3 $5 }

IterStmt : 'while' '(' RExpr ')' Stmt { While $3 $5 }
         | 'do' Stmt 'while' '(' RExpr ')' ';'          { DoWhile $2 $5 }


{
newtype Ident = Ident String deriving (Eq, Ord, Show, Read)
data Boolean = Boolean_True | Boolean_False
  deriving (Eq, Ord, Show, Read)

data RExpr
    = Or RExpr RExpr
    | And RExpr RExpr
    | Not RExpr
    | Eq RExpr RExpr
    | Neq RExpr RExpr
    | Lt RExpr RExpr
    | LtE RExpr RExpr
    | Gt RExpr RExpr
    | GtE RExpr RExpr
    | Add RExpr RExpr
    | Sub RExpr RExpr
    | Mul RExpr RExpr
    | Div RExpr RExpr
    | Mod RExpr RExpr
    | Pow RExpr RExpr
    | Neg RExpr
    | Ref LExpr
    | FCall FunCall
    | Int Integer
    | Char Char
    | String String
    | Float Double
    | Bool Boolean
    | Lexpr LExpr
  deriving (Eq, Ord, Show, Read)

data FunCall = Call Ident [RExpr]
  deriving (Eq, Ord, Show, Read)

data LExpr
    = Deref RExpr
    | PreInc LExpr
    | PreDecr LExpr
    | PostInc LExpr
    | PostDecr LExpr
    | BasLExpr BLExpr
  deriving (Eq, Ord, Show, Read)

data BLExpr = ArrayEl BLExpr RExpr | Id Ident
  deriving (Eq, Ord, Show, Read)

data Program = Prog [Decl]
  deriving (Eq, Ord, Show, Read)

data Decl
    = DvarBInit BasicType [VarDeclInit]
    | DvarCInit TypeSpec [VarDeclInit]
    | Dfun BasicType Ident [Parameter] CompStmt
  deriving (Eq, Ord, Show, Read)

data TypeSpec = BasTyp BasicType | CompType CompoundType
  deriving (Eq, Ord, Show, Read)

data BasicType
    = BasicType_bool
    | BasicType_char
    | BasicType_float
    | BasicType_int
    | BasicType_void
  deriving (Eq, Ord, Show, Read)

data CompoundType
    = ArrDef TypeSpec Integer | ArrUnDef TypeSpec | Pointer TypeSpec
  deriving (Eq, Ord, Show, Read)

data VarDeclInit = VarDeclIn Ident ComplexRExpr
  deriving (Eq, Ord, Show, Read)

data ComplexRExpr = Simple RExpr | Array [ComplexRExpr]
  deriving (Eq, Ord, Show, Read)

data Parameter = Param Modality TypeSpec Ident
  deriving (Eq, Ord, Show, Read)

data Modality
    = ModalityEmpty
    | Modality_val
    | Modality_ref
    | Modality_const
    | Modality_res
    | Modality_valres
    | Modality_name
  deriving (Eq, Ord, Show, Read)

data CompStmt
    = CompStmt1 CompStmtNocolon
    | CompStmtCompStmtNocolon CompStmtNocolon
  deriving (Eq, Ord, Show, Read)

data CompStmtNocolon = BlockDecl [Decl] [Stmt]
  deriving (Eq, Ord, Show, Read)

data Stmt
    = Comp CompStmt
    | ProcCall FunCall
    | Jmp JumpStmt
    | Iter IterStmt
    | Sel SelectionStmt
    | Assgn LExpr Assignment_op RExpr
    | LExprStmt LExpr
  deriving (Eq, Ord, Show, Read)

data Assignment_op
    = Assign
    | AssgnMul
    | AssgnAdd
    | AssgnDiv
    | AssgnSub
    | AssgnPow
    | AssgnAnd
    | AssgnOr
  deriving (Eq, Ord, Show, Read)

data JumpStmt = Break | Continue | RetExpVoid | RetExp RExpr
  deriving (Eq, Ord, Show, Read)

data SelectionStmt = IfNoElse RExpr Stmt | IfElse RExpr Stmt Stmt
  deriving (Eq, Ord, Show, Read)

data IterStmt = While RExpr Stmt | DoWhile Stmt RExpr
  deriving (Eq, Ord, Show, Read)


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))
}
