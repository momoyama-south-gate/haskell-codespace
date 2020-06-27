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
-- prop> identity "abc" == "abc"
identity :: a -> a
identity a = a

-- |
-- prop> const "abc" 5 == "abc"
const :: a -> b -> a
const a _ = a

-- |
-- prop> ((+1) ((*2) 2)) == ((+1) . (*2)) 2
-- prop> (+1) ((*2) ((+4) 4)) == ((+1) . (*2) . (+4)) 4
(.) :: (b -> c) -> (a -> b) -> a -> c
(.) fbc fab a = fbc (fab a)

infixr 9 .

-- |
-- prop> (+) 1 2 == flip (+) 2 1
flip :: (a -> b -> c) -> b -> a -> c
flip fab b a = fab a b

-- |
-- prop> ((+1) ((*2) 2)) == ((+1) $ (*2) 2)
($) :: (a -> b) -> a -> b
($) fab a = fab a

infixr 0 $

-- |
-- prop> (*2) ((+1) 1) == (1 & (+1) & (*2))
(&) :: a -> (a -> b) -> b
(&) = flip ($)

infixl 1 &
