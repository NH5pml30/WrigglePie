module Eval where

import Lexer ( alexScanTokens )
import Parser ( parseExpr )
import Grammar ( SideEffect )

computeSideEffects :: String -> [[SideEffect]]
computeSideEffects = parseExpr . alexScanTokens
