{
module MRDALexer where
}

%wrapper "basic"

$digit = 0-9        -- digits
$alpha = [a-zA-Z]   -- alphabetic characters

tokens :-
    $white+           	;
    $digit+         	{ \s -> Int (read s) }
    [\+\*\;]            { \s -> Sym (head s) }

{
-- Each action has type :: String -> Token
data Token = Sym Char | Int Int 
	deriving (Eq,Show)
}