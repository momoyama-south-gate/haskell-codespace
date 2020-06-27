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
<<<<<<< HEAD
<<<<<<< HEAD
fst (a, _) = a

snd :: (a, b) -> b
snd (_, b) = b
=======
fst (a, b) = a

snd :: (a, b) -> b
snd (a, b) = b
>>>>>>> code move
=======
fst (a, _) = a

snd :: (a, b) -> b
snd (_, b) = b
>>>>>>> homework2

curry :: ((a, b) -> c) -> a -> b -> c
curry f a b = f (a, b)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (a, b) = f a b

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
  (a, b) <> (c, d) = (a <> c, b <> d)

instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty = (mempty, mempty)

instance Functor ((,) a) where
  fmap f (a, b) = (a, f b)
-- fmap::(a -> b)-> f a -> f b

-- |
-- prop> prop_Applicative_Id @((,) (Sum Int))
-- prop> prop_Applicative_Comp @((,) (Sum Int))
-- prop> prop_Applicative_Homo @((,) (Sum Int)) Proxy
-- prop> prop_Applicative_Inter @((,) (Sum Int))
instance Monoid a => Applicative ((,) a) where
  pure b = (mempty, b)
-- pure :: a -> f a
-- <*> :: (a, (b -> c)) -> (a, b) -> (a, c)
-- u <*> pure y = pure ($ y) <*> u
-- lhs = (1, const 0) <*> (0, 0) = (0, 0)
-- rhs = (0, \f -> f 0) <*> (1, const 0) = (1, 0)
  (<*>) fbc fb = (fst fbc, (snd fbc) (snd fb)) 
-- <*> :: (a, (b -> c)) -> (a, b) -> (a, c)

instance Monoid a => Monad ((,) a)
