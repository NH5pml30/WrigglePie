{
module Parser where

import Lexer
import Grammar
}

%name parseExpr
%tokentype { Token }
%error { parseError }

%token
    var     { TokenVar $$ }
    '->'    { TokenImplies }
    '&'     { TokenAnd }
    '|'     { TokenOr }
    '!'     { TokenNot }
    '('     { TokenLeftPar }
    ')'     { TokenRightPar }

%%

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
