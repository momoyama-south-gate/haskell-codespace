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
import My.Data.Function
import qualified Prelude as P

-- data Maybe a = Nothing | Just a

maybe b f ma =
  case ma of
    Just a -> f a
    Nothing -> b

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing ma = not $ isJust ma

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just a) = a
fromMaybe a1 _ = a1

listToMaybe :: [a] -> Maybe a
listToMaybe l =
  case l of
    [] -> Nothing
    a : _ -> Just a

maybeToList :: Maybe a -> [a]
maybeToList (Just a) = [a]
maybeToList _ = []

-- |
-- >>> catMaybes [Just 1, Just 2, Nothing]
-- [1,2]
-- >>> catMaybes []
-- []
-- >>> catMaybes [Nothing, Just 1, Just 2]
-- [1,2]
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Just a : tl) = a : catMaybes tl
catMaybes (_ : tl) = catMaybes tl

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f l = catMaybes $ P.map f l

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


instance Alternative Maybe where
  empty = Nothing
  Just a <|> _ = Just a
  _ <|> Just b = Just b
  _ <|> _ = Nothing   
-- empty = f a
-- <|>:: f a -> f a -> f a

-- |
-- prop> prop_Monad_Left_Id @Maybe
-- prop> prop_Monad_Right_Id @Maybe
-- prop> prop_Monad_Assoc @Maybe
instance Monad Maybe where
  Just a >>= f = f a
  Nothing >>= _ = Nothing
