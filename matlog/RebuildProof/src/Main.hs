module Main where

import Grammar ( Proof (..) )
import Lexer ( alexScanTokens )
import Parser ( parseFile )
import qualified ProofTree ( treeifyHilbertProof, TreeifyResult (..) )

main :: IO ()
main = do
    input <- getContents
    case parseFile $ alexScanTokens input of
        proof@(Proof context statement statements)
            | not (null statements) && last statements == statement ->
                case ProofTree.treeifyHilbertProof context statements of
                    (ProofTree.Success proofTree) -> print proofTree
                    (ProofTree.Failure line) -> putStrLn $ "Proof is incorrect at line " ++ show (line + 2)
            | otherwise -> putStrLn "The proof does not prove the required expression"
