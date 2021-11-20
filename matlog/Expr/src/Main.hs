module Main where

import Grammar (Expr (..))
import Lexer (alexScanTokens)
import Parser (parseExpr)

main :: IO ()
main = getContents >>= print . parseExpr . alexScanTokens
