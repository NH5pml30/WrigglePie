module Main where

import Eval

main :: IO ()
main = do
    str <- getContents
    mapM_ print $ computeSideEffects str
