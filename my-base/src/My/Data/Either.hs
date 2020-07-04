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
import My.Control.Monad
import My.Data.Function
import My.Data.Functor
import My.Data.Semigroup
import My.Prelude.Internal
import My.Data.Tuple

either :: (a -> c) -> (b -> c) -> Either a b -> c
either fa _ (Left a) = fa a
either _ fb (Right b) = fb b

lefts :: [Either a b] -> [a]
lefts [] = []
lefts (Left a : tl) = a : lefts tl
lefts (_ : tl) = lefts tl

rights :: [Either a b] -> [b]
rights [] = []
rights (Right b : tl) = b : rights tl
rights (_ : tl) = rights tl

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

isRight :: Either a b -> Bool
isRight = not . isLeft

fromLeft :: a -> Either a b -> a
fromLeft _ (Left a) = a
fromLeft a _ = a

fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _ = b

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers [] = ([], [])
partitionEithers (Left a : tl) = (a : first, second)
  where
    (first, second) = partitionEithers tl
partitionEithers (Right a : tl) = (first, a : second)
  where
    (first, second) = partitionEithers tl

instance Semigroup (Either a b) where
  Left a <> Left b = Left a
  Left a <> Right b = Right b
  Right a <> _ = Right a

instance Functor (Either e) where
  fmap _ (Left e) = Left e
  fmap f (Right a) = Right (f a)

instance Applicative (Either e) where
  pure = Right
  (<*>) fab fa =
    case (fab, fa) of
      (Right f, Right a) -> Right (f a)
      (Left e, _) -> Left e
      (Right f, Left e) -> Left e

instance Monad (Either e) where
  Right a >>= f = f a
  Left e >>= _ = Left e

-- >>= :: m a -> (a -> m b) -> m b
