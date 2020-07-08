module My.Data.Functor.Compose
  ( module X,
  )
where

import Data.Functor.Compose as X
  ( Compose (..),
  )
import My.Control.Applicative
import My.Data.Functor

-- newtype Compose f g a
--   = Compose
--       { getCompose :: f (g a)
--       }

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap h (Compose f) = Compose (fmap (fmap h) f)
-- Compose (fmap (\g -> fmap h g) f)
-- (a -> b) -> f a -> f b
-- (a -> b) -> f (g a) -> f (g b)
-- a -> b -> g a -> g b

instance (Alternative f, Applicative g) => Alternative (Compose f g) where
  empty = Compose empty
  (<|>) (Compose fga) (Compose fgb) = Compose (fga <|> fgb)

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure a = Compose (pure(pure a))
  (<*>) (Compose fgh) (Compose fga) = Compose (pure (\g -> (<*>) g) <*> fgh <*> fga) 
-- f(g a -> g b)) -> f(g a) -> f(g b)
-- g (a -> b) -> g a -> g b
-- f (g (a -> b) -> g a -> g b) -> f(g(a->b)) -> (f(g a -> g b)) 
