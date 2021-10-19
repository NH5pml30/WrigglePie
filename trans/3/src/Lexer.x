{
module Lexer where
}

%wrapper "posn"

$digit = [0-9]
$alpha = [a-zA-Z_]

tokens :-

    $white+                     ;
    $digit+                     { tokenFactory $ \s -> TokenNumber (read s :: Integer) }
    $alpha [$alpha $digit]*     { tokenFactory $ \s -> TokenVar s }
    =                           { tokenFactory $ \_ -> TokenAssign }
    \+                          { tokenFactory $ \_ -> TokenPlus }
    \-                          { tokenFactory $ \_ -> TokenMinus }
    \*                          { tokenFactory $ \_ -> TokenMul }
    \/                          { tokenFactory $ \_ -> TokenDiv }
    \%                          { tokenFactory $ \_ -> TokenMod }
    \(                          { tokenFactory $ \_ -> TokenLeftPar }
    \)                          { tokenFactory $ \_ -> TokenRightPar }
    \;                          { tokenFactory $ \_ -> TokenSemicolon }

{

tokenFactory :: (String -> TokenData) -> AlexPosn -> String -> Token
tokenFactory dataFactory p s = Token p (dataFactory s)

data TokenData
    = TokenNumber Integer
    | TokenVar String
    | TokenAssign
    | TokenPlus
    | TokenMinus
    | TokenMul
    | TokenDiv
    | TokenMod
    | TokenLeftPar
    | TokenRightPar
    | TokenSemicolon
    deriving Show

data Token = Token AlexPosn TokenData

parseError :: [Token] -> a
parseError tokens = error ("Parse error at " ++ loc ++ "\n")
        where loc = case tokens of
                [] -> "end of file"
                token:_ -> show l ++ ":" ++ show c
                        where (Token (AlexPn _ l c) _) = token

}
