{-# LANGUAGE ScopedTypeVariables #-}

module My.Data.Semigroup
  ( Semigroup (..),
    prop_Semigroup_Assoc,
  )
where

import My.Prelude.Internal

class Semigroup a where
  (<>) :: a -> a -> a

infixr 6 <>

-- | Semigroup 法则

-- | 结合律（associativity）
-- a <> (b <> c) 等于 (a <> b) <> c
prop_Semigroup_Assoc :: forall a. (Eq a, Semigroup a) => a -> a -> a -> Bool
prop_Semigroup_Assoc a b c = (a <> b) <> c == a <> (b <> c)

instance Semigroup b => Semigroup (a -> b)
-- (<>) :: (a -> b) -> (a -> b) -> (a -> b)
