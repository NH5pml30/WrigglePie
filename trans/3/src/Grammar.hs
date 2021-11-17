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

assignAction :: String -> Maybe Integer -> ClosureHistory -> State ClosureHistory (Maybe Integer)
assignAction lhs rhs h = case rhs of
    Just x -> modify (closureAssign lhs x) >> return rhs
    Nothing -> return $ Just $ h ! lhs

safeDiv :: Integer -> Integer -> Maybe Integer
safeDiv _ 0 = Nothing
safeDiv a b = Just $ a `div` b

composeTwo :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
composeTwo = (.) . (.)
