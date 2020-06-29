{-# LANGUAGE ScopedTypeVariables #-}

module My.Data.Monoid
  ( module X,
    Monoid (..),
    prop_Monoid_LeftId,
    prop_Monoid_RightId,
    mconcat,
  )
where

import Data.Monoid as X (Sum(..))
import My.Data.Semigroup
import My.Prelude.Internal

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

-- |
-- >>> mconcat [Sum 1, Sum 2, Sum 3]
-- Sum {getSum = 6}
-- >>> mconcat ([] :: [Sum a])
-- Sum {getSum = 0}
-- >>> mconcat ["a", "b", "c"]
-- "abc"
-- >>> mconcat ([] :: [String])
-- ""
mconcat :: Monoid a => [a] -> a
mconcat l =
  case l of
    [] -> mempty

instance Monoid b => Monoid (a -> b) where
  mempty _ = mempty
-- mempty :: a -> b

instance Num a => Monoid (Sum a) where
  mempty = Sum 0

instance Monoid [a] where
  mempty = []
