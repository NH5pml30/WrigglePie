{
{-# LANGUAGE FlexibleContexts #-}

module Parser where

import Lexer
import Grammar

import Control.Monad.State
import Control.Monad
import Data.Map ( Map, empty )
import Data.Maybe

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
%attribute lvalue { Maybe String }

%%

File        : Statements                 { $$ = evalState $1 (ClosureHistory empty []) }

Statements  : Statement ';'              { $$ = $1 >> gets ((: []) . closureToSideEffects) }
            | Statement ';' Statements   { $$ = liftM2 (:) ($1 >> gets closureToSideEffects) (modify clearHistory >> $3) }

Statement   : Expr                       { $$ = $1; $$.lvalue = $1.lvalue }
            | Expr '=' Statement         { $$ = $1 >> do {r <- $3; h <- get; assignAction (fromJust $1.lvalue) r h}
                                         ; $$.lvalue = $1.lvalue
                                         }

Expr        : Term                       { $$ = $1; $$.lvalue = $1.lvalue }
            | Expr '+' Term              { $$ = liftM2 (liftA2 (+)) $1 $3; $$.lvalue = Nothing }
            | Expr '-' Term              { $$ = liftM2 (liftA2 (-)) $1 $3; $$.lvalue = Nothing }

Term        : Factor                     { $$ = $1; $$.lvalue = $1.lvalue }
            | Term '*' Factor            { $$ = liftM2 (liftA2 (*)) $1 $3; $$.lvalue = Nothing }
            | Term '/' Factor            { $$ = liftM2 (join `composeTwo` liftA2 safeDiv) $1 $3; $$.lvalue = Nothing }
            | Term '%' Factor            { $$ = liftM2 (liftA2 mod) $1 $3; $$.lvalue = Nothing }

Factor      : '-' Factor                 { $$ = (negate <$>) <$> $2; $$.lvalue = Nothing }
            | '+' Factor                 { $$ = $2; $$.lvalue = Nothing }
            | var                        { $$ = gets (Just . (! $1)); $$.lvalue = Just $1 }
            | n                          { $$ = gets (Just . (const $1)); $$.lvalue = Nothing }
            | '(' Statement ')'          { $$ = $2; $$.lvalue = $2.lvalue }
