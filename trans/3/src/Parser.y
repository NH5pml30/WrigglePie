{
{-# LANGUAGE FlexibleContexts #-}

module Parser where

import Lexer
import Grammar

import Control.Monad.State
import Control.Monad.Reader
import Data.Map
}

%name parseExpr
%tokentype { Token }
%error { parseError }

%token
    n       { Token _ (TokenNumber $$) }
    var     { Token _ (TokenVar $$) }
    '='     { Token _ TokenAssign }
    '+'     { Token _ TokenPlus }
    '-'     { Token _ TokenMinus }
    '*'     { Token _ TokenMul }
    '/'     { Token _ TokenDiv }
    '%'     { Token _ TokenMod }
    '('     { Token _ TokenLeftPar }
    ')'     { Token _ TokenRightPar }
    ';'     { Token _ TokenSemicolon }

%attributetype { Context a }
%attribute value { a }
%attribute varMapIn { Map String Integer }
%attribute varMapOut { Map String Integer }
%attribute computed { Integer }

%%

File        : Statements           { $$ = $1
                                   ; $1.varMapIn = empty
                                   }

Statements  : Statement            { $$ = [$1]
                                   ; $1.varMapIn = $$.varMapIn
                                   }
            | Statement Statements { $$ = $1 : $2
                                   ; $1.varMapIn = $$.varMapIn
                                   ; $2.varMapIn = $1.varMapOut
                                   }

Statement   : var '=' Expr ';'     { $3.computed = runReader $3 $$.varMapIn
                                   ; $$ = SideEffect $1 $3.computed
                                   ; $$.varMapOut = insert $1 $3.computed $$.varMapIn
                                   }

Expr        : Term                 { $$ = $1 }
            | Expr '+' Term        { $$ = liftM2 (+) $1 $3 }
            | Expr '-' Term        { $$ = liftM2 (-) $1 $3 }

Term        : Factor               { $$ = $1 }
            | Term '*' Factor      { $$ = liftM2 (*) $1 $3 }
            | Term '/' Factor      { $$ = liftM2 div $1 $3 }
            | Term '%' Factor      { $$ = liftM2 mod $1 $3 }

Factor      : '-' Factor           { $$ = negate <$> $2 }
            | '+' Factor           { $$ = $2 }
            | var                  { $$ = asks (! $1) }
            | n                    { $$ = asks (const $1) }
            | '(' Expr ')'         { $$ = $2 }
