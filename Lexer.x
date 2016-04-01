{
{-# OPTIONS -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -w #-}
module Lexer where

import qualified Data.Bits
import Data.Word (Word8)
import Data.Char (ord)    
}

$alpha = [a-zA-Z\192 - \255] # [\215 \247]      -- isolatin1 letter FIXME
$capital = [A-Z\192-\221] # [\215]              -- capital isolatin1 letter FIXME
$small = [a-z\222-\255] # [\247]                -- small isolatin1 letter FIXME
$digit = [0-9]                                  -- digit
$ident = [$alpha $digit _ ']                    -- identifier character
$universal = [\0-\255]                          -- universal: any character

-- symbols and non-identifier-like reserved words
@reservedSyms =
    \( | \) | \| \| | \& \& | \! | \= \= | \! \= | \< | \< \= |
    \> | \> \= | \+ | \- | \* | \/ | \% | \^ | \& |
    \, | \+ \+ | \- \- | \[ | \] | \; | \= | \{ | \} |
    \* \= | \+ \= | \/ \= | \- \= | \: | \< \- | \= \>

tokens :-
    -- Toss single line commenTokenSymbols
    "//"[.]*                                        ;
    "/*"([$universal # \*]
        | \*+ [$universal # [\* \/]])* ("*")+ "/"   ;
    $white+                                         ;

    @reservedSyms                           {  \p s -> Token p (TokenSymbols s) }
    $digit+                                 {  \p s -> Token p (TokenInt s) }
    $digit+ \. $digit+ (e (\-)? $digit+)?   {  \p s -> Token p (TokenDouble s) }
    "to"                                    {  \p s -> Token p (TokenSymbols s) }
    "ex"                                    {  \p s -> Token p (TokenSymbols s) }
    "try"                                   {  \p s -> Token p (TokenSymbols s) }
    "catch"                                 {  \p s -> Token p (TokenSymbols s) }
    "case"                                  {  \p s -> Token p (TokenSymbols s) }
    "false"                                 {  \p s -> Token p (TokenSymbols s) }
    "true"                                  {  \p s -> Token p (TokenSymbols s) }
    "Boolean"                               {  \p s -> Token p (TokenSymbols s) }
    "break"                                 {  \p s -> Token p (TokenSymbols s) }
    "Char"                                  {  \p s -> Token p (TokenSymbols s) }
    "continue"                              {  \p s -> Token p (TokenSymbols s) }
    "do"                                    {  \p s -> Token p (TokenSymbols s) }
    "else"                                  {  \p s -> Token p (TokenSymbols s) }
    "Float"                                 {  \p s -> Token p (TokenSymbols s) }
    "Array"                                 {  \p s -> Token p (TokenSymbols s) }
    "String"                                {  \p s -> Token p (TokenSymbols s) }
    "if"                                    {  \p s -> Token p (TokenSymbols s) }
    "Int"                                   {  \p s -> Token p (TokenSymbols s) }
    "return"                                {  \p s -> Token p (TokenSymbols s) }
    "val"                                   {  \p s -> Token p (TokenSymbols s) }
    "var"                                   {  \p s -> Token p (TokenSymbols s) }
    "valres"                                {  \p s -> Token p (TokenSymbols s) }
    "Unit"                                  {  \p s -> Token p (TokenSymbols s) }
    "while"                                 {  \p s -> Token p (TokenSymbols s) }
    "for"                                   {  \p s -> Token p (TokenSymbols s) }
    "def"                                   {  \p s -> Token p (TokenSymbols s) }
    $alpha $ident*                          {  \p s -> Token p (TokenIdent s) }
    \" ([$universal # [\" \\ \n]]
        | (\\ (\" | \\ | \' | n | t)))* \"  {  \p s -> Token p (TokenAlpha s) }
    \' ($universal # [\' \\]
        | \\ [\\ \' n t]) \'                {  \p s -> Token p (TokenChar s) }

{
data Tok =
    TokenSymbols !String            -- reserved words and symbols
    | TokenAlpha !String            -- string literals
    | TokenInt !String              -- integer literals
    | TokenIdent !String            -- identifiers
    | TokenDouble !String           -- double precision float literals
    | TokenChar !String             -- character literals
    deriving (Eq,Show,Ord)

data Token =
    Token {tpos::Posn, tok::Tok}
    | Err {tPost::Posn}
    deriving (Eq,Show,Ord)

tokenPos :: [Token] -> String
tokenPos (Token (Pn l _) _ :_) = "line " ++ show l
tokenPos (Err (Pn l _) :_) = "line " ++ show l
tokenPos _ = "end of file"

tokenPosn :: Token -> Posn
tokenPosn (Token p _) = p
tokenPosn (Err p) = p

tokenLineCol :: Token -> (Int, Int)
tokenLineCol = posLineCol . tokenPosn

posLineCol :: Posn -> (Int, Int)
posLineCol (Pn l c) = (l,c)

prToken :: Token -> String
prToken t = case t of
    Token _ (TokenSymbols s) -> s
    Token _ (TokenAlpha s)   -> show s
    Token _ (TokenInt s)   -> s
    Token _ (TokenIdent s)   -> s
    Token _ (TokenDouble s)   -> s
    Token _ (TokenChar s)   -> s


-------------------------------------------------------------------
-- Alex wrapper code.
-- A modified "posn" wrapper.
-------------------------------------------------------------------

data Posn = Pn !Int !Int
      deriving (Eq, Show,Ord)

alexStartPos :: Posn
alexStartPos = Pn 1 1

alexMove :: Posn -> Char -> Posn
alexMove (Pn l c) '\t' = Pn l     (((c+7) `div` 8)*8+1)
alexMove (Pn l c) '\n' = Pn (l+1) 1
alexMove (Pn l c) _    = Pn l     (c+1)

type Byte = Word8

type AlexInput = (Posn,     -- current position,
                  Char,     -- previous char
                  [Byte],   -- pending bytes on the current char
                  String)   -- current input string

parseTokens :: String -> [Token]
parseTokens str = go (alexStartPos, '\n', [], str)
    where
      go :: AlexInput -> [Token]
      go inp@(pos, _, _, str) =
               case alexScan inp 0 of
                AlexEOF                   -> []
                AlexError (pos, _, _, _)  -> [Err pos]
                AlexSkip  inp' len        -> go inp'
                AlexToken inp' len act    -> act pos (take len str) : (go inp')

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p, c, (b:bs), s) = Just (b, (p, c, bs, s))
alexGetByte (p, _, [], s) =
  case  s of
    []  -> Nothing
    (c:s) ->
             let p'     = alexMove p c
                 (b:bs) = utf8Encode c
              in p' `seq` Just (b, (p', c, bs, s))

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p, c, bs, s) = c

-- utf8Encode function definition
-- Encodes a Haskell String to a list of Word8 values, in UTF8 format.
-- If you are using the a wrapper that takes a Haskell String as input,
-- this function is automatically generated by Alex.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
    where
        go oc
           | oc <= 0x7f       = [oc]
           | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                                , 0x80 + oc Data.Bits..&. 0x3f
                                ]
           | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                                , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                                , 0x80 + oc Data.Bits..&. 0x3f
                                ]
           | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                                , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                                , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                                , 0x80 + oc Data.Bits..&. 0x3f
                                ]
}
