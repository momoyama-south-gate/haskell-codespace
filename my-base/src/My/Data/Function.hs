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

-- |
-- prop> identity x == x
identity :: a -> a
identity x = x

-- |
-- >>> const 1 "" 
-- 1
-- >>> const "abc" 2
-- "abc"

-- |
-- prop> const x y == x
const :: a -> b -> a
const x _ = x 

-- |
-- >>> ( identity . identity ) 1
-- 1
-- >>> ( (+1) . (+2) ) 1
-- 4
(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g x = f (g x)

infixr 9 .

-- |
-- >>> flip (-) 1 2
-- 1
flip :: (a -> b -> c) -> b -> a -> c
flip f y x = f x y

-- |
-- >>> (+) 10 $ 2+1
-- 13
-- >>> (-) 10 $ 2+1
-- 7
($) :: (a -> b) -> a -> b
($) f x = f x

infixr 0 $

-- |
-- >>> 10 & (+1)
-- 11
-- >>> 10 & (-) 1
-- -9
(&) :: a -> (a -> b) -> b
(&) = flip ($)

infixl 1 &
