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
import My.Data.Monoid
import My.Data.Semigroup
import My.Prelude.Internal

-- data Maybe a = Nothing | Just a

maybe b f ma =
  case ma of
    Just a -> f a
    Nothing -> b

isJust :: Maybe a -> Bool
isJust ma =
  case ma of
    Just a -> True
    Nothing -> False

isNothing :: Maybe a -> Bool
isNothing ma =
  case ma of
    Just a -> False
    Nothing -> True

fromMaybe :: a -> Maybe a -> a
fromMaybe a1 ma =
  case ma of
    Just a -> a
    _ -> a1

listToMaybe :: [a] -> Maybe a
listToMaybe l =
  case l of
    [] -> Nothing
    a : tl -> Just a

maybeToList :: Maybe a -> [a]
maybeToList ma =
  case ma of
    Just a -> [a]
    Nothing -> []

catMaybes :: [Maybe a] -> [a]
catMaybes l =
  case l of
    [] -> []
    ma : tl ->
      case ma of
        Just a -> a : (catMaybes tl)
        Nothing -> (catMaybes tl)


mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f l =
  case l of
    [] -> []
    a : tl ->
      case (f a) of
        Just b -> b : (mapMaybe f tl)
        Nothing -> mapMaybe f tl

instance Semigroup a => Semigroup (Maybe a) where
  Just a <> Nothing = Just a
  Just a <> Just b = Just (a <> b)
  Nothing <> Just b = Just b
  Nothing <> Nothing = Nothing

instance Semigroup a => Monoid (Maybe a) where
  mempty = Nothing

instance Functor Maybe where
  fmap f ma =
    case ma of
      Just a -> Just (f a)
      Nothing -> Nothing

instance Applicative Maybe where
  pure = Just
  (<*>) mf ma =
    case (mf,ma) of
      (Just f, Just a) -> Just (f a)
      _ -> Nothing
-- <*>::f(a -> b) -> f a -> f b

instance Alternative Maybe

instance Monad Maybe