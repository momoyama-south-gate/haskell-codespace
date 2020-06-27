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

instance (Functor f, Functor g) => Functor (Compose f g)

instance (Alternative f, Applicative g) => Alternative (Compose f g)

instance (Applicative f, Applicative g) => Applicative (Compose f g)
