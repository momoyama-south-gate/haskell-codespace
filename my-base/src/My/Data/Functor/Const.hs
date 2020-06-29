module My.Data.Functor.Const
  ( module X,
  )
where

import Data.Functor.Const as X (Const (..))
import My.Control.Applicative
import My.Data.Functor
import My.Data.Monoid
import My.Data.Semigroup
import My.Prelude.Internal
import My.Test.Arbitrary

-- newtype Const c a
--   = Const
--       { getConst :: c
--       }

instance Semigroup c => Semigroup (Const c a) where
  Const a <> Const b = Const (a <> b)

instance Monoid c => Monoid (Const c a) where
  mempty = Const mempty

instance Functor (Const c) where
  fmap _ (Const c) = Const c
