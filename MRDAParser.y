{
module MRDAParser where
import MRDALexer
}

%name parseTokens
%tokentype { Token }
%error { parseError }

%token
    int     { TokenInt $$ }
    var     { TokenString $$ }
    'char'  { TokenType ($$, "char") }
    'bool'  { TokenType ($$, "bool") }
    'int'   { TokenType ($$, "int") }
    'if'    { TokenReservedWord ($$, "if") }
    'else'  { TokenReservedWord ($$, "else") }
    'true'  { TokenReservedWord ($$, "true") }
    'false' { TokenReservedWord ($$, "false") }
    '+'     { TokenSymbol ($$, '+') }
    '*'     { TokenSymbol ($$, '*') }
    ';'     { TokenSymbol ($$, ';') }
    '('     { TokenSymbol ($$, '(') }
    ')'     { TokenSymbol ($$, ')') }
    '{'     { TokenSymbol ($$, '{') }
    '}'     { TokenSymbol ($$, '}') }
    '-'     { TokenSymbol ($$, '-') }
    '/'     { TokenSymbol ($$, '/') }
    '>='    { TokenBool ($$, ">=") }
    '<='    { TokenBool ($$, "<=") }
    '=='    { TokenBool ($$, "==") }
    '!='    { TokenBool ($$, "!=") }
    '='     { TokenBool ($$, "=") }
    '>'     { TokenBool ($$, ">") }
    '<'     { TokenBool ($$, "<") }

%left '+' '-'
%left '*' '/'
%nonassoc '<' '>' '<=' '>=' '==' '!='
%left NEG

%%

PROGRAM : BLOCK PROGRAM     {Program $1 $2}
    | {- empty -}           {ProgramEmpty}

BLOCK : DECLARATION    {BlockDecl $1}
    | BLOCKIF           {BlockIf $1}

BLOCKIF : 'if' '(' EXP ')' '{' BLOCK '}' 'else' '{' BLOCK '}'     {StatementIfElse $3 $6 $10}
    | 'if' '(' EXP ')' '{' BLOCK '}'                              {StatementIf $3 $6}

DECLARATION: TYPES var '=' EXP ';'     {DeclarationExp $1 (snd $2) $4}

TYPES : 'bool'                      {TypeBool}
    | 'char'                        {TypeChar}
    | 'int'                         {TypeInt}

EXP : EXP '==' EXP                  {BoolEqualOp $1 $3}
    | EXP '!=' EXP                  {BoolNEqOP $1 $3}
    | EXP '>=' EXP                  {BoolGEqOP $1 $3}
    | EXP '<=' EXP                  {BoolLEqOP $1 $3}
    | EXP '>' EXP                   {BoolGreaterOp $1 $3}
    | EXP '<' EXP                   {BoolLessOP $1 $3}
    | EXP '+' EXP                   {PlusOP $1 $3}
    | EXP '*' EXP                   {TimesOP $1 $3}
    | EXP '/' EXP                   {DivideOP $1 $3}
    | EXP '-' EXP                   {MinusOP $1 $3}
    | '(' EXP ')'                   {Bracket $2}
    | '-' EXP %prec NEG             {NegateOP $2}
    | int                           {Int (snd $1)}
    | 'true'                          {TrueVal}
    | 'false'                         {FalseVal}
    | var                           {Var (snd $1)}

{
parseError :: [Token] -> a
parseError token = error $ "Parse error" ++ (show token)

data ABS = TYPES | DECLARATION | PROGRAM | EXP
    deriving (Show)

data TYPES = TypeInt | TypeChar | TypeBool
    deriving (Show)

data PROGRAM = Program BLOCK PROGRAM | ProgramEmpty
    deriving (Show)

data BLOCK = BlockDecl DECLARATION | BlockIf BLOCKIF
    deriving (Show)

data BLOCKIF = StatementIfElse EXP BLOCK BLOCK | StatementIf EXP BLOCK
    deriving (Show) 

data DECLARATION = DeclarationExp TYPES String EXP
    deriving (Show)

data EXP = GrtOp EXP EXP
    | BoolGreaterOp EXP EXP
    | BoolLessOP EXP EXP 
    | BoolEqualOp EXP EXP
    | BoolNEqOP EXP EXP
    | BoolGEqOP EXP EXP
    | BoolLEqOP EXP EXP
    | PlusOP EXP EXP
    | TimesOP EXP EXP
    | Int Int
    | MinusOP EXP EXP
    | Bracket EXP
    | NegateOP EXP
    | DivideOP EXP EXP
    | TrueVal
    | FalseVal
    | Var String
    deriving (Show)
}