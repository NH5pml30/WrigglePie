module Grammar where

import Control.Monad.State
import Data.Map ( insert, Map )
import qualified Data.Map ( empty, (!) )
import Data.Semigroup (Last (Last))

data SideEffect = SideEffect String Integer
instance Show SideEffect where
    show (SideEffect name val) = name ++ " = " ++ show val

data ClosureHistory = ClosureHistory (Map String Integer) [SideEffect]

closureAssign :: String -> Integer -> ClosureHistory -> ClosureHistory
closureAssign lhs rhs (ClosureHistory varmap history) = ClosureHistory (insert lhs rhs varmap) (SideEffect lhs rhs : history)

closureToSideEffects :: ClosureHistory -> [SideEffect]
closureToSideEffects (ClosureHistory _ a) = reverse a

(!) :: ClosureHistory -> String -> Integer
(ClosureHistory varmap _) ! k = varmap Data.Map.! k

clearHistory :: ClosureHistory -> ClosureHistory
clearHistory (ClosureHistory varmap _) = ClosureHistory varmap []

assignAction :: String -> Integer -> State ClosureHistory Integer
assignAction lhs rhs = modify (closureAssign lhs rhs) >> return rhs
