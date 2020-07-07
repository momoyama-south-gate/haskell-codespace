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
import My.Data.List
import My.Data.Proxy
import My.Data.Functor
import My.Data.Semigroup
import My.Prelude.Internal
import Data.Bool (not)

either :: (a -> c) -> (b -> c) -> Either a b -> c
either fl _ (Left x) = fl x
either _ fr (Right y) = fr y

lefts :: [Either a b] -> [a]
lefts xs = case xs of
  [] -> []
  (Left x):xs -> x:(lefts xs)
  (Right _):xs -> lefts xs

rights :: [Either a b] -> [b]
rights xs = case xs of
  [] -> []
  (Left _):xs -> rights xs
  (Right x):xs -> x:(rights xs)

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

isRight :: Either a b -> Bool
isRight x = not (isLeft x)

fromLeft :: a -> Either a b -> a
fromLeft _ (Left x) = x
fromLeft n (Right _) = n

fromRight :: b -> Either a b -> b
fromRight n (Left _) = n
fromRight _ (Right x) = x

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers xs = (lefts xs, rights xs)

-- |
-- -- prop> prop_Semigroup_Assoc @(Either (Sum Int) (Sum Int))
instance Semigroup (Either a b) where
  -- <> :: Either a b -> Either a b -> Either a b
  x <> Left _ = x
  Right x <> _ = Right x
  Left _ <> Right y = Right y

-- |
-- -- prop> prop_Functor_Id @(Either String)
-- -- prop> prop_Functor_Comp @(Either String)
instance Functor (Either e) where
  -- fmap :: (a -> b) -> Either e a -> Either e b
  fmap _ (Left e) = Left e
  fmap f (Right x) = Right (f x)

-- |
-- -- prop> prop_Applicative_Id @(Either String)
-- -- prop> prop_Applicative_Comp @(Either String)
-- -- prop> prop_Applicative_Homo @(Either String) Proxy
-- -- prop> prop_Applicative_Inter @(Either String) 
instance Applicative (Either e) where
  -- pure :: a -> Either e a
  pure x = Right x
  -- <*> :: Either e (a -> b) -> Either e a -> Either e b
  Right f <*> Right x = Right (f x)
  Left e <*> _ = Left e
  _ <*> Left e = Left e

-- -- |
-- prop> prop_Monad_Assoc @(Either Int)
-- prop> prop_Monad_Left_Id @(Either Int)
-- prop> prop_Monad_Right_Id @(Either Int)
instance Monad (Either e) where
  -- (>>=) :: Either e a -> (a -> Either e b) -> Either e b
  Left e >>= _ = Left e
  Right x >>= f = f x
