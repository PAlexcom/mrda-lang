{
module MRDALexer where
}

%wrapper "basic"

$digit = 0-9        -- digits
$alpha = [a-zA-Z]   -- alphabetic characters

tokens :-
    $white+     ;
    $digit+     { \s -> TokenInt (read s) }
    [\+\*\;]    { \s -> TokenSymbol (head s) }

{
-- Each action has type :: String -> Token
data Token = TokenSymbol Char
    | TokenInt Int
    deriving (Eq,Show)
}