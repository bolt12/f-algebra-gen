module TH where

import Language.Haskell.TH
import Control.Monad
import Data.Char
import Data.Functor.Foldable

-- (1) Main generation function -----
makeCombinator :: Name -> Q [Dec]
makeCombinator t = do
    TyConI (DataD _ _ _ _ constructors _) <- reify t

    pe <- genPE "f" (length constructors)

    let combName = mkName . map toLower . nameBase $ t
        clauses  = map (combinClause pe) $ zip constructors [0..]
    r <- funD combName clauses
    return [r]

-----

-- (2) makeCombinator auxiliary function -----
-- Generates a single function clause

combinClause :: ([PatQ], [ExpQ]) -- Function arguments pattern
             -> (Con, Int) -- (Constructor, Indice of current constructor)
             -> ClauseQ
combinClause (patsF, varsF) (NormalC name fields, i)         = do
    (patsC, varsC) <- genPE "a" (length fields)
    funClause patsF varsF patsC varsC name (length fields) i
combinClause (patsF, varsF) (RecC name fields, i)            = do
    (patsC, varsC) <- genPE "a" (length fields)
    funClause patsF varsF patsC varsC name (length fields) i
combinClause (patsF, varsF) (InfixC _ name _, i)             = do
    (patsC, varsC) <- genPE "a" 2
    funClause patsF varsF patsC varsC name 2 i
combinClause _ (ForallC{}, _) = error "makeCombinator: GADTs are not currently supported."
combinClause _ (GadtC{}, _) = error "makeCombinator: GADTs are not currently supported."
combinClause _ (RecGadtC{}, _) = error "makeCombinator: GADTs are not currently supported."

-----

-- (3) combinClause auxiliary functions -----
funClause :: [PatQ] -> [ExpQ] -> [PatQ] -> [ExpQ] -> Name -> Int -> Int -> ClauseQ
funClause pF vF pC vC name l i = 
    clause (pF ++ [conP name pC]) 
           (normalB (appE (vF !! i) 
                          (applyConVars vC name vC (l - 1)))) 
           []

applyConVars :: [ExpQ] -> t -> [a] -> Int -> ExpQ
applyConVars _ _ [] _             = conE (mkName "()")
applyConVars varsC _ [_] n        = varsC !! n
applyConVars varsC name' (_:fs) n = tupE (applyConVars varsC name' fs (n-1) : [varsC !! n])

------

-- Generate n unique variables and return them in form of patterns and expressions
genPE :: String -> Int -> Q ([PatQ], [ExpQ])
genPE x n = do
  ids <- replicateM n (newName x)
  return (map varP ids, map varE ids)
