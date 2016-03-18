{
module MRDALexer where
}

$alpha = [a-zA-Z\192 - \255] # [\215 \247]      -- isolatin1 letter FIXME
$capital = [A-Z\192-\221] # [\215]              -- capital isolatin1 letter FIXME
$small = [a-z\222-\255] # [\247]                -- small isolatin1 letter FIXME
$digit = [0-9]                                  -- digit
$ident = [$alpha $digit _ ']                    -- identifier character
$universal = [\0-\255]                          -- universal: any character

-- symbols and non-identifier-like reserved words
@reservedSyms =
    \( | \) | \| \| | \& \& | \! | \= \= | \! \= | \< | \<
    \= | \> | \> \= | \+ | \- | \* | \/ | \% | \^ | \& |
    \, | \+ \+ | \- \- | \[ | \] | \; | \= | \{ | \} |
    \* \= | \+ \= | \/ \= | \- \=

tokens :-
    -- Toss single line commenTokenSymbols
    "//"[.]*                                        ;
    "/*"([$universal # \*]
        | \*+ [$universal # [\* \/]])* ("*")+ "/"   ;
    $white+                                         ;

    @reservedSyms                           {  \p s -> Token p (TokenSymbols s) }
    $alpha $ident*                          {  \p s -> Token p (TokenIdent s) }
    \" ([$universal # [\" \\ \n]]
        | (\\ (\" | \\ | \' | n | t)))* \"  {  \p s -> Token p (TokenAlpha s) }
    \' ($universal # [\' \\]
        | \\ [\\ \' n t]) \'                {  \p s -> Token p (TokenChar s)) }
    $digit+                                 {  \p s -> Token p (TokenInt s)) }
    $digit+ \. $digit+ (e (\-)? $digit+)?   {  \p s -> Token p (TokenDouble s)) }
    'False'                                 {  \p s -> Token p (TokenSymbols s) }
    'True'                                  {  \p s -> Token p (TokenSymbols s) }
    'bool'                                  {  \p s -> Token p (TokenSymbols s) }
    'break'                                 {  \p s -> Token p (TokenSymbols s) }
    'char'                                  {  \p s -> Token p (TokenSymbols s) }
    'const'                                 {  \p s -> Token p (TokenSymbols s) }
    'continue'                              {  \p s -> Token p (TokenSymbols s) }
    'do'                                    {  \p s -> Token p (TokenSymbols s) }
    'else'                                  {  \p s -> Token p (TokenSymbols s) }
    'float'                                 {  \p s -> Token p (TokenSymbols s) }
    'if'                                    {  \p s -> Token p (TokenSymbols s) }
    'int'                                   {  \p s -> Token p (TokenSymbols s) }
    'name'                                  {  \p s -> Token p (TokenSymbols s) }
    'ref'                                   {  \p s -> Token p (TokenSymbols s) }
    'res'                                   {  \p s -> Token p (TokenSymbols s) }
    'return'                                {  \p s -> Token p (TokenSymbols s) }
    'val'                                   {  \p s -> Token p (TokenSymbols s) }
    'valres'                                {  \p s -> Token p (TokenSymbols s) }
    'void'                                  {  \p s -> Token p (TokenSymbols s) }
    'while'                                 {  \p s -> Token p (TokenSymbols s) }

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
    Token Posn Tok
    | Err Posn
    deriving (Eq,Show,Ord)

tokenPos :: [Token] -> String
tokenPos (Token (Pn _ l _) _ :_) = "line " ++ show l
tokenPos (Err (Pn _ l _) :_) = "line " ++ show l
tokenPos _ = "end of file"

tokenPosn :: Token -> Posn
tokenPosn (Token p _) = p
tokenPosn (Err p) = p

tokenLineCol :: Token -> (Int, Int)
tokenLineCol = posLineCol . tokenPosn

posLineCol :: Posn -> (Int, Int)
posLineCol (Pn _ l c) = (l,c)

prToken :: Token -> String
prToken t = case t of
    Token _ (TokenSymbols s _) -> s
    Token _ (TokenAlpha s)   -> show s
    Token _ (TokenInt s)   -> s
    Token _ (TokenIdent s)   -> s
    Token _ (TokenDouble s)   -> s
    Token _ (TokenChar s)   -> s


-------------------------------------------------------------------
-- Alex wrapper code.
-- A modified "posn" wrapper.
-------------------------------------------------------------------

data Posn = Pn !Int !Int !Int
      deriving (Eq, Show,Ord)

alexStartPos :: Posn
alexStartPos = Pn 0 1 1

alexMove :: Posn -> Char -> Posn
alexMove (Pn a l c) '\t' = Pn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (Pn a l c) '\n' = Pn (a+1) (l+1)   1
alexMove (Pn a l c) _    = Pn (a+1)  l     (c+1)

type Byte = Word8

type AlexInput = (Posn,     -- current position,
                  Char,     -- previous char
                  [Byte],   -- pending bytes on the current char
                  String)   -- current input string

tokens :: String -> [Token]
tokens str = go (alexStartPos, '\n', [], str)
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

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.BiTokenSymbols.shiftR` 6)
                        , 0x80 + oc Data.BiTokenSymbols..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.BiTokenSymbols.shiftR` 12)
                        , 0x80 + ((oc `Data.BiTokenSymbols.shiftR` 6) Data.BiTokenSymbols..&. 0x3f)
                        , 0x80 + oc Data.BiTokenSymbols..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.BiTokenSymbols.shiftR` 18)
                        , 0x80 + ((oc `Data.BiTokenSymbols.shiftR` 12) Data.BiTokenSymbols..&. 0x3f)
                        , 0x80 + ((oc `Data.BiTokenSymbols.shiftR` 6) Data.BiTokenSymbols..&. 0x3f)
                        , 0x80 + oc Data.BiTokenSymbols..&. 0x3f
                        ]
}