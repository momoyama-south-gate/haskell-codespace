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
import My.Data.Functor
import My.Data.Monoid
import My.Data.Semigroup
import My.Prelude.Internal

-- data Maybe a = Nothing | Just a

maybe :: b -> (a -> b) -> Maybe a -> b
maybe = undefined

isJust :: Maybe a -> Bool
isJust = undefined

isNothing :: Maybe a -> Bool
isNothing = undefined

fromMaybe :: a -> Maybe a -> a
fromMaybe = undefined

listToMaybe :: [a] -> Maybe a
listToMaybe = undefined

maybeToList :: Maybe a -> [a]
maybeToList = undefined

catMaybes :: [Maybe a] -> [a]
catMaybes = undefined

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe = undefined

instance Semigroup a => Semigroup (Maybe a)

instance Semigroup a => Monoid (Maybe a)

instance Functor Maybe

instance Applicative Maybe
