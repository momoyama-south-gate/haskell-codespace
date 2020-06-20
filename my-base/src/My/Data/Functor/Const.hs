module My.Data.Functor.Const
  ( module X,
  )
where

import Data.Functor.Const as X (Const (..))
import My.Control.Applicative
import My.Data.Functor
import My.Data.Monoid
import My.Data.Semigroup
import My.Data.Void
import My.Prelude.Internal
import My.Test.Arbitrary

-- newtype Const c a
--   = Const
--       { getConst :: c
--       }

-- |
-- -- prop> prop_Semigroup_Assoc @(Const (Sum Int) String)
instance Semigroup c => Semigroup (Const c a) where
  -- <> :: Const c -> Const c -> Const c
  (Const x) <> (Const y) = Const (x <> y)

-- |
-- -- prop> prop_Monoid_LeftId @(Const (Sum Int) String)
-- -- prop> prop_Monoid_RightId @(Const (Sum Int) String)
instance Monoid c => Monoid (Const c a) where
  -- mempty :: Const c 
  mempty = Const mempty

-- |
-- -- prop> prop_Functor_Comp @(Const String)
-- -- prop> prop_Functor_Id @(Const String)
instance Functor (Const c) where
  -- fmap :: forall a b. (a -> b) -> Const c a -> Const c b
  fmap _ (Const c) = Const c
