module My.Data.Tuple
  ( fst,
    snd,
    curry,
    uncurry,
    swap,
  )
where

import My.Control.Applicative
import My.Control.Monad
import My.Data.Functor
import My.Data.Function
import My.Data.Monoid
import My.Data.Semigroup
import My.Prelude.Internal

fst :: (a, b) -> a
fst (x,_) = x

snd :: (a, b) -> b
snd (_,y) = y

curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x,y)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x,y) = f x y

swap :: (a, b) -> (b, a)
swap (x,y) = (y,x)

-- |
-- -- prop> prop_Semigroup_Assoc @(Sum Int,Sum Int)
-- -- prop> prop_Monoid_LeftId @(Sum Int,Sum Int)
-- -- prop> prop_Monoid_RightId @(Sum Int,Sum Int)
-- -- prop> prop_Functor_Id @((,)Sum Int)
-- -- prop> prop_Functor_Comp @((,) Sum Int)
-- -- prop> prop_Applicative_Id @((,) Sum Int)
-- -- prop> prop_Applicative_Comp @((,) Sum Int)
-- -- prop> prop_Applicative_Homo @((,) Sum Int) Proxy
-- -- prop> prop_Applicative_Inter @((,) Sum Int) 
instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
  -- <> :: (a,b) -> (a,b) -> (a,b)
  (a1, b1) <> (a2, b2) = (a1 <> a2, b1 <> b2)

instance (Monoid a, Monoid b) => Monoid (a, b) where
  -- mempty :: (a,b)
  mempty = (mempty, mempty)

instance Functor ((,) a) where
  -- fmap :: (b -> c) -> (a,b) -> (a,c)
  fmap f (x,y) = (x, (f y))

instance Monoid a => Applicative ((,) a) where
  -- pure :: b -> (a, b)
  pure x = (mempty, x)

instance Monoid a => Monad ((,) a)
  -- <*> :: (a,(b -> c)) -> (a,b) -> (a,c)
  (a1,f) <*> (a2,b) = ((a1 <> a2), (f b))
