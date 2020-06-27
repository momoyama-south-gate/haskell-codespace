{-# LANGUAGE ScopedTypeVariables #-}

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
import My.Data.Monoid
import My.Data.Semigroup
import My.Prelude.Internal
import My.Data.Function
import My.Data.Proxy
import Data.Functor.Identity

fst :: (a, b) -> a
fst (a, _) = a

snd :: (a, b) -> b
snd (_, b) = b

curry :: ((a, b) -> c) -> a -> b -> c
curry f a b = f (a, b)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (a, b)= f a b

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
  (a, b) <> (c, d) = (a <> c, b <> d)

instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty = (mempty, mempty)

instance Functor ((,) a) where
  fmap f fa = (fst fa, f $ snd fa)
-- fmap::(a -> b)-> f a -> f b

instance Monoid a => Applicative ((,) a) where
  pure b = (mempty, b)
-- pure :: a -> f a
  (<*>) fbc fb = (fst fbc, (snd fbc) (snd fb)) 
-- <*> :: (a, (b -> c)) -> (a, b) -> (a, c)

instance Monoid a => Monad ((,) a)
