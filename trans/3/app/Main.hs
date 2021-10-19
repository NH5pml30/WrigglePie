module Main where

import Eval
import Data.List (intercalate)

main :: IO ()
main = do
    str <- getContents
    mapM_ (putStrLn . intercalate ", " . map show) $ computeSideEffects str
