{-# LANGUAGE ScopedTypeVariables #-}

module My.Data.Functor
  ( Functor (..),
    prop_Functor_Comp,
    prop_Functor_Id,
    (<$),
    ($>),
    (<$>),
    (<&>),
    void,
  )
where

import My.Data.Function
import My.Prelude.Internal
import My.Test.Arbitrary

class Functor f where
  fmap :: (a -> b) -> f a -> f b

-- | Functor 法则

-- | 组成律（composition）
-- fmap (g . f) 等于 (fmap g) . (fmap f)
prop_Functor_Comp ::
  forall f a b c.
  (Functor f, Eq (f c)) =>
  Fun a b ->
  Fun b c ->
  f a ->
  Bool
prop_Functor_Comp fab fbc fa = fmap (g . f) fa == ((fmap g) . (fmap f)) fa
  where
    f :: a -> b
    f = applyFun fab
    g :: b -> c
    g = applyFun fbc

-- | 同一律（identity）
-- fmap identity 等于 identity
prop_Functor_Id :: forall f a. (Functor f, Eq (f a)) => f a -> Bool
prop_Functor_Id fa = fmap identity fa == identity fa

(<$) :: Functor f => a -> f b -> f a
(<$) a = fmap $ const a

infixl 4 <$

($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)

infixl 4 $>

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

infixl 4 <$>

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)

infixl 1 <&>

void :: Functor f => f a -> f ()
void = (<$) ()

instance Functor ((->) a) where
  fmap = (.)
-- fmap :: (b -> c) -> (a -> b) -> (a -> c)
