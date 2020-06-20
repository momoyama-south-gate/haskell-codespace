module My.Data.Functor.Identity
  ( module X,
  )
where

import Data.Functor.Identity as X (Identity (..))
import My.Control.Applicative
import My.Control.Monad
import My.Data.Functor
import My.Data.Monoid
import My.Data.Semigroup
import My.Data.Proxy
import My.Prelude.Internal

-- newtype Identity a
--   = Identity
--       { runIdentity :: a
--       }

-- |
-- -- prop> prop_Semigroup_Assoc @(Identity (Sum Int))
instance Semigroup a => Semigroup (Identity a) where
  -- (<>) :: Identity a -> Identity a -> Identity a
  (Identity x) <> (Identity y) = Identity (x <> y)

-- |
-- -- prop> prop_Monoid_LeftId @(Sum Int)
-- -- prop> prop_Monoid_RightId @(Sum Int)
-- -- prop> prop_Monoid_LeftId @(Identity (Sum Int))
-- -- prop> prop_Monoid_RightId @(Identity (Sum Int))

instance Monoid a => Monoid (Identity a) where
  -- mempty :: Monoid a => a
  mempty = Identity mempty

-- |
-- -- prop> prop_Functor_Comp @Identity
-- -- prop> prop_Functor_Id @Identity
instance Functor Identity where
  -- fmap :: Functor f => (a -> b) -> f a -> f b
  -- fmap :: (a -> b) -> Identity a -> Identity b 
  fmap f (Identity a) = Identity (f a)

-- |
-- -- prop> prop_Applicative_Id @Identity
-- -- prop> prop_Applicative_Comp @Identity
-- -- prop> prop_Applicative_Homo @Identity Proxy
-- -- prop> prop_Applicative_Inter @Identity 
instance Applicative Identity where
  -- pure :: a -> Identity a
  pure x = Identity x
  -- (<*>) :: Identity (a -> b) -> Identity a -> Identity b
  (Identity f) <*> (Identity a) = Identity (f a)

instance Monad Identity