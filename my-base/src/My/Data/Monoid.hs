{-# LANGUAGE ScopedTypeVariables #-}

module My.Data.Monoid
  ( Monoid (..),
    prop_Monoid_LeftId,
    prop_Monoid_RightId,
    mconcat,
  )
where

import My.Data.Semigroup
import My.Prelude.Internal
import My.Data.Function

class Semigroup a => Monoid a where
  mempty :: a

-- | Monoid 法则

-- | 左同一律（left identity）
-- mempty <> a 等于 a
prop_Monoid_LeftId :: forall a. (Eq a, Monoid a) => a -> Bool
prop_Monoid_LeftId a = mempty <> a == a

-- | 右同一律（right identity）
-- a <> mempty 等于 a
prop_Monoid_RightId :: forall a. (Eq a, Monoid a) => a -> Bool
prop_Monoid_RightId a = a <> mempty == a

mconcat :: Monoid a => [a] -> a
mconcat [] = mempty
mconcat (x:xs) = x <> (mconcat xs)

instance Monoid b => Monoid (a -> b) where
-- mempty :: a -> b
  mempty = const mempty

instance Num a => Monoid (Sum a) where
  mempty = Sum 0
