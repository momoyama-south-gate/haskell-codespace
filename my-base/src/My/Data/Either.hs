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
either fa fb eab =
  case eab of
    Left a -> fa a
    Right b -> fb b

lefts :: [Either a b] -> [a]
lefts l =
  case l of
    [] -> []
    eab : tl ->
      case eab of
        Left a -> a : lefts tl
        _ -> lefts tl

rights :: [Either a b] -> [b]
rights l =
  case l of
    [] -> []
    eab : tl ->
      case eab of
        Right b -> b : rights tl
        _ -> rights tl

isLeft :: Either a b -> Bool
isLeft eab =
  case eab of
    Left a -> True
    _ -> False

isRight :: Either a b -> Bool
isRight eab =
  case eab of
    Right a -> True
    _ -> False

fromLeft :: a -> Either a b -> a
fromLeft a1 eab =
  case eab of
    Left a -> a
    _ -> a1

fromRight :: b -> Either a b -> b
fromRight b1 eab =
  case eab of
    Right b -> b
    _ -> b1

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers l =
  case l of
    [] -> ([], [])
    eab : tl ->
      let
        other = partitionEithers tl
      in
        case eab of
          Left a -> (a : (fst other), snd other)
          Right b -> (fst other, b : (snd other))  

instance Semigroup (Either a b) where
  Left a <> Left b = Left a
  Left a <> Right b = Right b
  Right a <> Left b = Right a
  Right a <> Right b = Right a

instance Functor (Either e) where
  fmap f eb =
    case eb of
      Left e -> Left e
      Right a -> Right (f a)

instance Applicative (Either e) where
  pure = Right
  (<*>) fab fa =
    case (fab, fa) of
      (Right f, Right a) -> Right (f a)
      (Left e, _) -> Left e
      (Right f, Left e) -> Left e

instance Monad (Either e)
