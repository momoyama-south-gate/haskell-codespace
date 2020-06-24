{-# LANGUAGE ScopedTypeVariables #-}

module My.Data.Semigroup
  ( module X,
    Semigroup (..),
    prop_Semigroup_Assoc,
  )
where

import My.Prelude.Internal
import Data.Semigroup as X
  ( Sum(..)
  )
import My.Data.Function

class Semigroup a where
  (<>) :: a -> a -> a

infixr 6 <>

instance Num a => Semigroup (Sum a) where
  Sum x <> Sum y = Sum $ x + y

-- | Semigroup 法则

-- |
-- -- prop> prop_Semigroup_Assoc @(Sum Int)

-- | 结合律（associativity）
-- a <> (b <> c) 等于 (a <> b) <> c
prop_Semigroup_Assoc :: forall a. (Eq a, Semigroup a) => a -> a -> a -> Bool
prop_Semigroup_Assoc a b c = (a <> b) <> c == a <> (b <> c)

instance Semigroup b => Semigroup (a -> b) where
-- (<>) :: (a -> b) -> (a -> b) -> (a -> b)
  ( f <> g ) x = (f x) <> (g x)
