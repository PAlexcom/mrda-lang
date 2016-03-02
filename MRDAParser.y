{
module MRDAParser where
import MRDALexer
}

%name parseTokens
%tokentype { Token }
%error { parseError }

%token
    int    { TokenInt $$ }
    '+'    { TokenSymbol ($$, '+') }
    '*'    { TokenSymbol ($$, '*') }
    ';'    { TokenSymbol ($$, ';') }

%%

EXP : FACTOR '+' FACTOR ';'  {PlusOP $1 $3}
    | FACTOR '*' FACTOR ';'  {TimesOP $1 $3}

FACTOR : int {Int (snd $1)}

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data FACTOR = Int Int
    deriving (Show)

data EXP = PlusOP FACTOR FACTOR | TimesOP FACTOR FACTOR
    deriving (Show)
}