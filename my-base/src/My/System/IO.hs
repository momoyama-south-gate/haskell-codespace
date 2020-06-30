module My.System.IO
  ( module X,
  )
where

import qualified Control.Applicative as A
import My.Control.Applicative
import My.Control.Monad
import My.Data.Functor
import System.IO as X
import qualified Prelude as P

instance Functor IO where
  fmap = P.fmap

instance Applicative IO where
  pure = P.pure
  (<*>) = (P.<*>)

instance Alternative IO where
  empty = A.empty
  (<|>) = (A.<|>)

instance Monad IO where
  (>>=) = (P.>>=)
