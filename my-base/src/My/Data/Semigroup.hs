{-# LANGUAGE ScopedTypeVariables #-}

module My.Data.Semigroup
  ( Semigroup (..),
    prop_Semigroup_Assoc,
  )
where

import Data.Monoid (Sum(..))
import My.Prelude.Internal

class Semigroup a where
  (<>) :: a -> a -> a

infixr 6 <>

-- | Semigroup 法则

-- | 结合律（associativity）
-- a <> (b <> c) 等于 (a <> b) <> c
prop_Semigroup_Assoc :: forall a. (Eq a, Semigroup a) => a -> a -> a -> Bool
prop_Semigroup_Assoc a b c = (a <> b) <> c == a <> (b <> c)

instance Semigroup b => Semigroup (a -> b) where
  (f <> g) x = f x <> g x
-- (<>) :: (a -> b) -> (a -> b) -> (a -> b)

instance Num a => Semigroup (Sum a) where
  Sum n <> Sum m = Sum (n + m)

instance Semigroup [a] where
  (<>) = (++)
    where
      (++) :: [a] -> [a] -> [a]
      (++) [] l = l
      (++) (h : tl) l = h : (tl ++ l)
