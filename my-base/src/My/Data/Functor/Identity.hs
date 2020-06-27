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

instance Semigroup a => Semigroup (Identity a)

instance Monoid a => Monoid (Identity a)

instance Functor Identity

instance Applicative Identity

instance Monad Identity
