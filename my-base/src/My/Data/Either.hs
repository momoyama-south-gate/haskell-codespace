module My.Data.Either
  ( module X,
    either,
    lefts,
    rights,
    isLeft,
    isRight,
    fromLeft,
    fromRight,
    partitionEithers,
  )
where

import Data.Either as X (Either (..))
import My.Control.Applicative
import My.Data.Function
import My.Data.Functor
import My.Data.Semigroup
import My.Prelude.Internal

either :: (a -> c) -> (b -> c) -> Either a b -> c
either = undefined

lefts :: [Either a b] -> [a]
lefts = undefined

rights :: [Either a b] -> [b]
rights = undefined

isLeft :: Either a b -> Bool
isLeft = undefined

isRight :: Either a b -> Bool
isRight = undefined

fromLeft :: a -> Either a b -> a
fromLeft = undefined

fromRight :: b -> Either a b -> b
fromRight = undefined

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = undefined

instance Semigroup (Either a b)

instance Functor (Either e)

instance Applicative (Either e)
