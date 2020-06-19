module My.Data.Function
  ( identity,
    const,
    (.),
    flip,
    ($),
    (&),
  )
where

import My.Prelude.Internal

identity :: a -> a
identity = undefined

const :: a -> b -> a
const = undefined

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) = undefined

infixr 9 .

flip :: (a -> b -> c) -> b -> a -> c
flip = undefined

($) :: (a -> b) -> a -> b
($) = undefined

infixr 0 $

(&) :: a -> (a -> b) -> b
(&) = undefined

infixl 1 &
