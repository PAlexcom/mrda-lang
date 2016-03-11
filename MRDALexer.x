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
    [\+\*\;\(\)\-\/\=]              { \pos s -> TokenSymbol (pos, (head s)) }
    "bool"                          { \pos s -> TokenType (pos, s) }
    "int"                           { \pos s -> TokenType (pos, s) }
    "char"                          { \pos s -> TokenType (pos, s) }
    $alpha [$alpha $digit \_ \’]*   { \pos s -> TokenString (pos, s) }

{
-- Each action has type :: String -> Token
data Token = TokenSymbol (AlexPosn, Char)
    | TokenInt (AlexPosn, Int)
    | TokenString (AlexPosn, String)
    | TokenType (AlexPosn, String)
    deriving (Eq,Show)
}