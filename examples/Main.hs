{-# LANGUAGE KindSignatures, TypeFamilies, DeriveFunctor, DeriveTraversable,
    DeriveFoldable, ExistentialQuantification #-}

module Main where

import TH
import Cp -- Program Calculus Combinators library
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Language.Haskell.TH
import Data.List (foldl')

makeCombinator ''ListF

l :: [a] -> Integer
l = cata (listf zero (succ . p2))

data BTree a = Empty | Node(a, (BTree a, BTree a)) deriving Show

makeBaseFunctor ''BTree
makeCombinator ''BTree
makeCombinator ''BTreeF

countBTree :: BTree a -> Integer
countBTree = cata (btreef (const 0) (succ . uncurry (+) . p2))

data Expr a
    = Lit a
    | Add (Expr a) (Expr a)
    | Expr a :* [Expr a]
    deriving (Show)

makeBaseFunctor ''Expr
makeCombinator ''Expr
makeCombinator ''ExprF

eval :: Expr Integer -> Integer
eval = cata (exprf id add (uncurry $ foldl' (*)))

expr1 :: Expr Integer
expr1 = Add (Lit 2) (Mul (Lit 3) [Lit 4])

data A a = C { v :: a, w :: a } | D { x :: a, z :: a }

makeCombinator ''A

data ExprInfix a
    = ExprInfix a :** [ExprInfix a]
    | AddI (ExprInfix a) (ExprInfix a)
    | LitI a
  deriving (Show)

makeCombinator ''ExprInfix

{- GADTs are not currently supported!
data B = forall a. Eq a => B [a]

makeCombinator ''B
-}

main :: IO ()
main = undefined

