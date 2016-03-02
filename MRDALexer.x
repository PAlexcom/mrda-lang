{
module MRDALexer where
}

%wrapper "basic"

$digit = 0-9        -- digits
$alpha = [a-zA-Z]   -- alphabetic characters

tokens :-
    $white+     ;
    $digit+     { \s -> TokenInt (read s) }
    [\+]        { \s -> TokenPlus (head s) }
    [\*]        { \s -> TokenTimes (head s) }
    [\;]        { \s -> TokenSemicolon (head s) }

{
-- Each action has type :: String -> Token
data Token = TokenPlus Char
    | TokenTimes Char
    | TokenSemicolon Char
    | TokenInt Int
    deriving (Eq,Show)
}