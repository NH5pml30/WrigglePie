{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}

import Test.HUnit

import Eval (computeSideEffects)
import Control.Exception
import Data.List (intercalate)
import Control.Monad
import Control.DeepSeq

computeOutput :: String -> [[String]]
computeOutput str = map (map show) $ computeSideEffects str

assertThrows :: String -> Assertion
assertThrows input = do
    let handler :: ErrorCall -> IO Bool
        handler e = return False
    nothrow <- (evaluate (force (computeOutput input)) >> return True) `catch` handler
    when nothrow $ assertFailure "Expected error"

tests :: Test
tests = test [
        "example" ~:
            [["a = 2"], ["b = 4"], ["c = 6"], ["a = 3"], ["c = 7"]] ~=?
            computeOutput "a = 2; b = a + 2; c = a + b * (b - 3); a = 3; c = a + b * (b - 3);",
        "basic" ~: test [
                "add" ~: [["a = 2"]] ~=? computeOutput "a = 1 + 1;",
                "subtract" ~: [["a = 0"]] ~=? computeOutput "a = 1 - 1;",
                "multiply" ~: [["a = 4"]] ~=? computeOutput "a = 2 * 2;",
                "divide" ~: [["a = 1"]] ~=? computeOutput "a = 4 / 3;",
                "mod" ~: [["a = 2"]] ~=? computeOutput "a = 6 % 4;",
                "parenthesis precedence" ~: [["a = 8"]] ~=? computeOutput "a = (2 + 2) * 2;",
                "no parenthesis precedence" ~: [["a = 6"]] ~=? computeOutput "a = 2 + 2 * 2;"
        ],
        "assoc" ~: test [
                "subtract" ~: [["a = -8"]] ~=? computeOutput "a = 1 - 2 - 3 - 4;",
                "divide" ~: [["a = 2"]] ~=? computeOutput "a = 24 / 4 / 3;",
                "assign" ~: [["c = 2", "b = 2", "a = 2"]] ~=? computeOutput "a = b = c = 2;"
        ],
        "naming" ~: test [
                "underscore" ~: [["_d = 1"]] ~=? computeOutput "_d = 1;",
                "long name" ~: [["____d____asnjf11413 = 2"]] ~=? computeOutput "____d____asnjf11413 = 2;"
        ],
        "whitespaces" ~: test [
                "no ws" ~: [["a = 2"]] ~=? computeOutput "a=(1+1*3)/2;",
                "multiple ws" ~: [["a = 2"]] ~=? computeOutput "a = (1 +   1   * \n  3)   / \t 2;"
        ],
        "lvalue expression" ~: [["a = 0", "a = 2"]] ~=? computeOutput "(a = 0) = 2;",
        "rvalue assignment" ~: [["a = 0", "b = 1", "a = 3"]] ~=? computeOutput "(a = 0) = (b = 1) + 2;",
        "error" ~: test [
                "rvalue assignment" ~: assertThrows "a + 1 = 0;",
                "uninitialized variable" ~: assertThrows "a = a;"
        ]
    ]

main :: IO ()
main = runTestTTAndExit tests
