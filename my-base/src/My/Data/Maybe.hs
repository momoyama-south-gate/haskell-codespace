module My.Data.Maybe
  ( module X,
    maybe,
    isJust,
    isNothing,
    fromMaybe,
    listToMaybe,
    maybeToList,
    catMaybes,
    mapMaybe,
  )
where

import Data.Maybe as X (Maybe (..))
import My.Control.Applicative
import My.Control.Monad
import My.Data.Functor
import My.Data.Function
import My.Data.List
import My.Data.Proxy
import My.Data.Monoid
import My.Data.Semigroup
import My.Prelude.Internal
import Data.Bool (not)

-- data Maybe a = Nothing | Just a

maybe :: b -> (a -> b) -> Maybe a -> b
maybe n _ Nothing = n
maybe _ f (Just x) = f x

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing x = not ( isJust x )

fromMaybe :: a -> Maybe a -> a
fromMaybe n Nothing = n
fromMaybe _ (Just x) = x

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- --|
-- prop> catMaybes [Nothing,Nothing] == []
-- prop> catMaybes [Nothing,Just 1] == [1]
-- prop> catMaybes [Just 2,Just 1] == [2,1]
-- prop> catMaybes [Just 1,Nothing] == [1]
catMaybes :: [Maybe a] -> [a]
catMaybes x = case x of
  [] -> []
  [xs] -> maybeToList xs
  Nothing:xs -> catMaybes xs
  (Just x):xs -> x:(catMaybes xs)

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f xs = catMaybes $ map f xs

-- |
-- -- prop> prop_Functor_Comp @(Maybe)
-- -- prop> prop_Semigroup_Assoc @(Maybe (Sum Int))
-- -- prop> prop_Monoid_LeftId @(Maybe (Sum Int))
-- -- prop> prop_Monoid_RightId @(Maybe (Sum Int))
-- -- prop> prop_Functor_Id @Maybe
-- -- prop> prop_Functor_Comp @(Maybe)
-- -- prop> prop_Applicative_Id @Maybe
-- -- prop> prop_Applicative_Comp @Maybe
-- -- prop> prop_Applicative_Homo @Maybe Proxy
-- -- prop> prop_Applicative_Inter @Maybe 
instance Semigroup a => Semigroup (Maybe a) where
  -- <> :: Maybe a -> Maybe a -> Maybe a
  (Just x) <> (Just y) = Just (x <> y)
  Nothing <> y = y
  x <> Nothing = x

instance Semigroup a => Monoid (Maybe a) where
  -- mempty :: Maybe a
  mempty = Nothing

instance Functor Maybe where
  -- fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just (f x)

instance Applicative Maybe where
  --pure :: a -> Maybe a
  pure x = Just x
  -- <*> :: Maybe (a -> b) -> Maybe a -> Maybe b
  (Just f) <*> (Just x) = Just (f x)
  _ <*> _ = Nothing

instance Alternative Maybe where
  -- empty :: Maybe a
  empty = Nothing
  -- (<|>) :: Maybe a -> Maybe a -> Maybe a
  (Just x) <|> _ = Just x
  Nothing <|> y = y

instance Monad Maybe where
  -- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  Nothing >>= _ = Nothing
  Just x >>= f = f x
