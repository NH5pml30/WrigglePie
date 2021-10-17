module Main where

import Lexer (alexScanTokens)
import Parser (parseExpr)
import Data.Map
import Control.Monad.State

main :: IO ()
main = do
    str <- getContents
    mapM_ print $ parseExpr (alexScanTokens str)
