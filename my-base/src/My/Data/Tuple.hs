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

fst :: (a, b) -> a
fst = undefined

snd :: (a, b) -> b
snd = undefined

curry :: ((a, b) -> c) -> a -> b -> c
curry = undefined

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry = undefined

swap :: (a, b) -> (b, a)
swap = undefined

instance (Semigroup a, Semigroup b) => Semigroup (a, b)

instance (Monoid a, Monoid b) => Monoid (a, b)

instance Functor ((,) a)

instance Monoid a => Applicative ((,) a)

instance Monoid a => Monad ((,) a)
