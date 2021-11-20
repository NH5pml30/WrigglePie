{
module Lexer where
}

%wrapper "basic"

$digit = [0-9]
$alpha = [A-Z]
$inlineWhite = [\ \t\f\v\r]

tokens :-
    $inlineWhite+               ;
    \n                          { \_ -> TokenNewline }
    $alpha [$alpha $digit ']*   { \s -> TokenVar s }
    "->"                        { \_ -> TokenImplies }
    &                           { \_ -> TokenAnd }
    \|                          { \_ -> TokenOr }
    !                           { \_ -> TokenNot }
    \(                          { \_ -> TokenLeftPar }
    \)                          { \_ -> TokenRightPar }
    "|-"                        { \_ -> TokenTurnstile}
    \,                          { \_ -> TokenComma }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token
    = TokenNewline
    | TokenVar String
    | TokenImplies
    | TokenAnd
    | TokenOr
    | TokenNot
    | TokenLeftPar
    | TokenRightPar
    | TokenTurnstile
    | TokenComma
    deriving (Show, Eq)
}
