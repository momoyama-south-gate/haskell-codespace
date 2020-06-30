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
import My.Prelude.Internal

-- newtype Identity a
--   = Identity
--       { runIdentity :: a
--       }

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity (a <> b)

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)
-- <*> :: f (a -> b) -> f a -> f b

instance Monad Identity
