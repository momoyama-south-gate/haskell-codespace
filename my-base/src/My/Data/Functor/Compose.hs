module My.Data.Functor.Compose
  ( module X,
  )
where

import Data.Functor.Compose as X
  ( Compose (..),
  )
import My.Control.Applicative
import My.Data.Functor
import My.Data.Function

-- newtype Compose f g a
--   = Compose
--       { getCompose :: f (g a)
--       }

instance (Functor f, Functor g) => Functor (Compose f g) where
  -- fmap :: (a -> b) -> Compose f g a -> Compose f g b
  fmap fab (Compose x) = Compose ((fab <$>) <$> x)

instance (Alternative f, Alternative g) => Alternative (Compose f g) where
  -- empty :: Compose f g a
  empty = Compose (pure empty)
  -- (<|>) :: Compose f g a -> Compose f g a -> Compose f g a
  Compose x <|> Compose y = Compose (liftA2 (<|>) x y)

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  -- pure :: a -> Compose f g a
  pure x = Compose (pure $ pure x)
  -- <*> :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  Compose fab <*> Compose x = Compose ((<*>) <$> fab <*> x)
