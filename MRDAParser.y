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
    '+'     { TokenSymbol ($$, '+') }
    '*'     { TokenSymbol ($$, '*') }
    ';'     { TokenSymbol ($$, ';') }
    '('     { TokenSymbol ($$, '(') }
    ')'     { TokenSymbol ($$, ')') }
    '-'     { TokenSymbol ($$, '-') }
    '/'     { TokenSymbol ($$, '/') }
    '='     { TokenSymbol ($$, '=') }

%left '+' '-'
%left '*' '/'
%left NEG

%%

PROGRAM : DECLARATION ';' PROGRAM   {Program $1 $3}
    | {- empty -}                   {ProgramEmpty}

DECLARATION: TYPES var '=' EXP      {DeclarationExp $1 (snd $2) $4}
    | TYPES var '=' var             {DeclarationString $1 (snd $2) (snd $4)}

TYPES : 'bool'                      {TypeBool}
    | 'char'                        {TypeChar}
    | 'int'                         {TypeInt}

EXP : EXP '+' EXP                   {PlusOP $1 $3}
    | EXP '*' EXP                   {TimesOP $1 $3}
    | EXP '/' EXP                   {DivideOP $1 $3}
    | EXP '-' EXP                   {MinusOP $1 $3}
    | '(' EXP ')'                   {Bracket $2}
    | '-' EXP %prec NEG             {NegateOP $2}
    | int                           {Int (snd $1)}

{
parseError :: [Token] -> a
parseError token = error $ "Parse error" ++ (show token)

data TYPES = TypeInt | TypeChar | TypeBool
    deriving (Show)

data DECLARATION = DeclarationExp TYPES String EXP
    | DeclarationString TYPES String String
    deriving (Show)

data PROGRAM = Program DECLARATION PROGRAM | ProgramEmpty
    deriving (Show)

data EXP = PlusOP EXP EXP
    | TimesOP EXP EXP
    | Int Int
    | MinusOP EXP EXP
    | Bracket EXP
    | NegateOP EXP
    | DivideOP EXP EXP
    deriving (Show)
}