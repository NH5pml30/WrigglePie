{
module Parser where

import Lexer
import Grammar
}

%name parseFile
%tokentype { Token }
%error { parseError }

%token
    '\n'    { TokenNewline }
    var     { TokenVar $$ }
    '->'    { TokenImplies }
    '&'     { TokenAnd }
    '|'     { TokenOr }
    '!'     { TokenNot }
    '('     { TokenLeftPar }
    ')'     { TokenRightPar }
    '|-'    { TokenTurnstile }
    ','     { TokenComma }

%%

File        : Context '|-' Exprs                 { Proof $1 (head $3) (tail $3) }

ContextPlus : Expr                               { [$1] }
            | Expr ',' ContextPlus               { $1 : $3 }

Context     : {- empty -}                        { [] }
            | ContextPlus                        { $1 }

Exprs       : Expr '\n'                          { [$1] }
            | Expr '\n' Exprs                    { $1 : $3 }

Expr        : Disjunction                        { $1 }
            | Disjunction '->' Expr              { Binary Implies $1 $3 }

Disjunction : Conjunction                        { $1 }
            | Disjunction '|' Conjunction        { Binary Or $1 $3 }

Conjunction : Negation                           { $1 }
            | Conjunction '&' Negation           { Binary And $1 $3 }

Negation    : '!' Negation                       { Unary Not $2 }
            | var                                { Var $1 }
            | '(' Expr ')'                       { $2 }

{}
