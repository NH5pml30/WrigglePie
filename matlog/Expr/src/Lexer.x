{
module Lexer where
}

%wrapper "basic"

$digit = [0-9]
$alpha = [A-Z]

tokens :-

    $white+                     ;
    $alpha [$alpha $digit ']*   { \s -> TokenVar s }
    "->"                        { \_ -> TokenImplies }
    &                           { \_ -> TokenAnd }
    \|                          { \_ -> TokenOr }
    !                           { \_ -> TokenNot }
    \(                          { \_ -> TokenLeftPar }
    \)                          { \_ -> TokenRightPar }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token
    = TokenVar String
    | TokenImplies
    | TokenAnd
    | TokenOr
    | TokenNot
    | TokenLeftPar
    | TokenRightPar
    deriving Show
}
