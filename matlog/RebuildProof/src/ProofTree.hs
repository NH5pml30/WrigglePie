{-# LANGUAGE TupleSections #-}
module ProofTree ( ProofTree (..), Rule (..), Expr (..), ContextExpr (..), TreeifyResult (..), treeifyHilbertProof ) where

import qualified Grammar (UnaryOp (..), BinaryOp (..), Expr (..))
import Data.Maybe ( fromJust, isJust, mapMaybe, listToMaybe )
import Data.List ( intercalate, delete, sort, find )
import Data.Tuple ()

data BinaryOp = Implies | Or | And deriving (Eq, Ord)
data Expr = Binary BinaryOp Expr Expr
          | Var String
          | LiteralFalse
          deriving (Eq, Ord)

fromGrammar (Grammar.Binary Grammar.Implies lhs rhs) = Binary Implies (fromGrammar lhs) (fromGrammar rhs)
fromGrammar (Grammar.Binary Grammar.Or lhs rhs)      = Binary Or (fromGrammar lhs) (fromGrammar rhs)
fromGrammar (Grammar.Binary Grammar.And lhs rhs)     = Binary And (fromGrammar lhs) (fromGrammar rhs)
fromGrammar (Grammar.Unary Grammar.Not arg)          = Binary Implies (fromGrammar arg) LiteralFalse
fromGrammar (Grammar.Var var)                        = Var var

instance Show BinaryOp where
    show Implies = "->"
    show Or      = "|"
    show And     = "&"
instance Show Expr where
    show (Binary op lhs rhs) = "(" ++ show lhs ++ show op ++ show rhs ++ ")"
    show (Var name)          = name
    show LiteralFalse        = "_|_"

data ContextExpr = ContextExpr [Expr] Expr
data Rule
    = Ax
    | EImplies
    | IImplies
    | IAnd
    | ElAnd
    | ErAnd
    | IlOr
    | IrOr
    | EOr
    | EFalse
    deriving Eq
data ProofTree = ProofTree Rule [ProofTree] ContextExpr

ruleTable =
    [(Ax,       "Ax"),
     (EImplies, "E->"),
     (IImplies, "I->"),
     (IAnd,     "I&"),
     (ElAnd,    "El&"),
     (ErAnd,    "Er&"),
     (IlOr,     "Il|"),
     (IrOr,     "Ir|"),
     (EOr,      "E|"),
     (EFalse,   "E_|_")]

printExpr (Grammar.Unary Grammar.Not arg) = "(" ++ printExpr arg ++ show Grammar.Implies ++ "_|_)"
printExpr expr = show expr

instance Show Rule where
    show = fromJust . flip lookup ruleTable
instance Show ContextExpr where
    show (ContextExpr context expr) = intercalate "," (map show context) ++ "|-" ++ show expr

printNode level (ProofTree rule premises conclusion) =
    unlines (map (printNode $ succ level) premises) ++
    "[" ++ show level ++ "] " ++ show conclusion ++ " [" ++ show rule ++ "]"

instance Show ProofTree where
    show = printNode 0

implies = Binary Implies
implies3 x y z = implies x $ implies y z
conj = Binary And
disj = Binary Or

listEqUpToOrder a b = sort a == sort b

tree rule premises context conclusion =
    ProofTree rule premises (ContextExpr context conclusion)

treeAx context phi | phi `elem` context = tree Ax [] context phi

treeEImplies premises@[
        ProofTree _ _ (ContextExpr context (Binary Implies phi psi)),
        ProofTree _ _ (ContextExpr context' phi')
    ] | context == context' && phi == phi'
    = tree EImplies premises context psi

treeIImplies premises@[ProofTree _ _ (ContextExpr context psi)] phi
    | phi `elem` context
    = tree IImplies premises (delete phi context) (phi `implies` psi)

treeIAnd premises@[
        ProofTree _ _ (ContextExpr context phi),
        ProofTree _ _ (ContextExpr context' psi)
    ] | context == context'
    = tree IAnd premises context (phi `conj` psi)

treeElAnd premises@[ProofTree _ _ (ContextExpr context (Binary And phi psi))]
    = tree ElAnd premises context phi

treeErAnd premises@[ProofTree _ _ (ContextExpr context (Binary And phi psi))]
    = tree ErAnd premises context psi

treeIlOr premises@[ProofTree _ _ (ContextExpr context phi)] psi = tree IlOr premises context (phi `disj` psi)

treeIrOr premises@[ProofTree _ _ (ContextExpr context psi)] phi = tree IrOr premises context (phi `disj` psi)

treeEOr premises@[
        ProofTree _ _ (ContextExpr context rho),
        ProofTree _ _ (ContextExpr context' rho'),
        ProofTree _ _ (ContextExpr context'' (Binary Or phi psi))
    ] | (context `listEqUpToOrder` (phi:context'')) && (context' `listEqUpToOrder` (psi:context'')) && rho == rho'
    = tree EOr premises context'' rho

treeEFalse premises@[ProofTree _ _ (ContextExpr context LiteralFalse)] = tree EFalse premises context

matchAxioms :: Expr -> Maybe ProofTree
matchAxioms statement = case statement of
    expr@(Binary Implies phi (Binary Implies psi phi')) | phi == phi'
        -> Just $
            treeIImplies
                [treeIImplies
                    [treeAx [psi, phi] phi] -- phi
                    psi] -- psi -> phi
                phi -- phi -> psi -> phi
    expr@(
        Binary Implies
            (Binary Implies phi psi)
            (Binary Implies
                (Binary Implies phi' (Binary Implies psi' pi))
                (Binary Implies phi'' pi'))
        ) | psi == psi' && phi == phi' && phi == phi'' && pi == pi'
        -> Just $
            treeIImplies
                [treeIImplies
                    [treeIImplies
                        [treeEImplies
                            [treeEImplies -- psi -> pi
                                [treeAx [phi, phipsi, phipsipi] phipsipi,
                                 treeAx [phi, phipsi, phipsipi] phi],
                             treeEImplies -- psi
                                [treeAx [phi, phipsi, phipsipi] phipsi,
                                 treeAx [phi, phipsi, phipsipi] phi]]] -- pi
                        phi] -- phi -> pi
                    phipsipi] -- (phi -> psi -> pi) -> (phi -> pi)
                phipsi -- (phi -> psi) -> (phi -> psi -> pi) -> (phi -> pi)
        where
            phipsi = phi `implies` psi
            phipsipi = implies3 phi psi pi
            phipi = phi `implies` pi
    expr@(
        Binary Implies phi (Binary Implies psi (Binary And phi' psi'))
        ) | phi == phi' && psi == psi'
        -> Just $
            treeIImplies
                [treeIImplies
                    [treeIAnd
                        [treeAx [phi, psi] phi, {-- phi --}
                         treeAx [phi, psi] psi] {-- psi --}] -- phi & psi
                    psi] -- psi -> phi & psi
                phi -- phi -> psi -> phi & psi
    expr@(Binary Implies (Binary And phi psi) phi') | phi == phi'
        -> Just $
            treeIImplies
                [treeElAnd
                    [treeAx [phiandpsi] phiandpsi] {-- phi & psi --}] -- phi
                phiandpsi -- phi & psi -> phi
        where
            phiandpsi = phi `conj` psi
    expr@(Binary Implies (Binary And phi psi) psi') | psi == psi'
        -> Just $
            treeIImplies
                [treeErAnd
                    [treeAx [phiandpsi] phiandpsi] {-- phi & psi --}] -- psi
                phiandpsi -- phi & psi -> psi
        where
            phiandpsi = phi `conj` psi
    expr@(Binary Implies phi (Binary Or phi' psi)) | phi == phi'
        -> Just $
            treeIImplies
                [treeIlOr
                    [treeAx [phi] phi] -- phi
                     psi] -- phi | psi
                phi -- phi -> phi | psi
    expr@(Binary Implies psi (Binary Or phi psi')) | psi == psi'
        -> Just $
            treeIImplies
                [treeIrOr
                    [treeAx [psi] psi] -- psi
                    phi] -- phi | psi
                psi -- psi -> phi | psi
    expr@(
        Binary Implies
            (Binary Implies phi pi)
            (Binary Implies
                (Binary Implies psi pi')
                (Binary Implies
                    (Binary Or phi' psi')
                    pi''))
        ) | phi == phi' && psi == psi' && pi == pi' && pi == pi''
        -> Just $
            treeIImplies
                [treeIImplies
                    [treeIImplies
                        [treeEOr
                            [treeEImplies
                                [treeAx [phipi, psipi, phiorpsi, phi] phipi, -- phi -> pi
                                 treeAx [phipi, psipi, phiorpsi, phi] phi {-- phi --}], -- pi
                            treeEImplies
                                [treeAx [phipi, psipi, phiorpsi, psi] psipi, -- psi -> pi
                                 treeAx [phipi, psipi, phiorpsi, psi] psi {-- psi --}], -- pi
                            treeAx [phipi, psipi, phiorpsi] phiorpsi {-- phi | psi --}] {-- pi --}]
                        phiorpsi {-- (phi | psi) -> pi --}]
                    psipi {-- (psi -> pi) -> ((phi | psi) -> pi) --}]
                phipi -- (phi -> pi) -> (psi -> pi) -> ((phi | psi) -> pi)
        where
            phipi = phi `implies` pi
            psipi = psi `implies` pi
            phiorpsi = phi `disj` psi
    -- (phi -> psi) -> (phi -> psi -> _|_) -> (phi -> _|_), 2 axiom applies
    expr@(
        Binary Implies
            phi
            (Binary Implies
                (Binary Implies phi' LiteralFalse)
                psi)
        ) | phi == phi'
        -> Just $
            treeIImplies
                [treeIImplies
                    [treeEFalse
                        [treeEImplies
                            [treeAx [phi, notphi] notphi, -- phi -> _|_
                             treeAx [phi, notphi] phi {-- phi --}] {-- _|_ --}]
                        psi {-- psi --}]
                    notphi {-- (phi -> _|_) -> psi --}]
                phi -- phi -> (phi -> _|_) -> psi
        where
            notphi = phi `implies` LiteralFalse
    _ -> Nothing

addContext :: ProofTree -> [Expr] -> ProofTree
addContext (ProofTree rule premises (ContextExpr context expr)) newContext =
    tree rule (map (`addContext` newContext) premises) (newContext ++ context) expr

proveAxiom :: [ProofTree] -> [Expr] -> Expr -> Maybe ProofTree
proveAxiom _ context statement = fmap (`addContext` context) (matchAxioms statement)

findImplication :: [ProofTree] -> Expr -> Expr -> Maybe ProofTree
findImplication [] premise statement = Nothing
findImplication (head:tail) premise statement =
    case head of
        (ProofTree _ _ (ContextExpr _ (Binary Implies premise' statement')))
            | premise == premise' && statement == statement' -> Just head
        _ -> findImplication tail premise statement

proveModusPonens :: [ProofTree] -> [Expr] -> Expr -> Maybe ProofTree
proveModusPonens provenStatements _ statement =
    let possible = listToMaybe $ mapMaybe
            (\arg@(ProofTree _ _ (ContextExpr context premise)) ->
                let implication = findImplication provenStatements premise statement
                in fmap (arg,) implication)
            provenStatements
    in case possible of
        (Just (premise, implication)) -> Just $ treeEImplies [implication, premise]
        _ -> Nothing

proveContext :: [ProofTree] -> [Expr] -> Expr -> Maybe ProofTree
proveContext _ context statement
    | statement `elem` context = Just $ treeAx context statement
    | otherwise = Nothing

computeFirstJustMaybe :: [a1] -> (a1 -> Maybe a2) -> Maybe a2
computeFirstJustMaybe fs applier = listToMaybe $ mapMaybe applier fs

proveStatement :: [ProofTree] -> [Expr] -> Expr -> Maybe ProofTree
proveStatement provenStatements context statement =
    computeFirstJustMaybe [proveContext, proveAxiom, proveModusPonens] (\f -> f provenStatements context statement)

proveProofHelper :: [ProofTree] -> Int -> [Expr] -> [Grammar.Expr] -> TreeifyResult
proveProofHelper (provenHead:provenTail) line context [] = Success provenHead
proveProofHelper [] line _ [] = Failure line
proveProofHelper provenStatements line context (headStt:stts) =
    let proof' = proveStatement provenStatements context (fromGrammar headStt)
    in case proof' of
        (Just proof) -> proveProofHelper (proof:provenStatements) (succ line) context stts
        _ -> Failure line

data TreeifyResult
    = Success ProofTree
    | Failure Int

treeifyHilbertProof :: [Grammar.Expr] -> [Grammar.Expr] -> TreeifyResult
treeifyHilbertProof context = proveProofHelper [] 0 (map fromGrammar context)
