{
module MRDALexer where
}

%wrapper "posn"

$digit = 0-9        -- digits
$alpha = [a-zA-Z]   -- alphabetic characters

tokens :-
    $white+                         ;
    "--".*                          ;
    $digit+                         { \pos s -> TokenInt (pos, (read s)) }
    [\+\*\;\(\)\-\/]                { \pos s -> TokenSymbol (pos, (head s)) }
    $alpha [$alpha $digit \_ \’]*   { \pos s -> TokenString (pos, (head s)) }

{
-- Each action has type :: String -> Token
data Token = TokenSymbol (AlexPosn, Char)
    | TokenInt (AlexPosn, Int)
    | TokenString (AlexPosn, String)
    deriving (Eq,Show)
}