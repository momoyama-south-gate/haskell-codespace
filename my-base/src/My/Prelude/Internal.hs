module My.Prelude.Internal
  ( module X,
    undefined,
  )
where

import Data.Bool as X
  ( (&&),
    Bool (..),
    not,
    otherwise,
    (||),
  )
import Data.Eq as X
  ( (/=),
    Eq (..),
  )
import Data.Int as X
  ( Int,
  )
import Data.Ord as X
  ( Ord (..),
  )
import Data.Ord as X
  ( (<),
    (<=),
    (>),
    (>=),
    Ordering (..),
    max,
    min,
  )
import Data.String as X
  ( String,
  )
import GHC.Num as X
  ( Num (..),
  )
import GHC.Show as X
  ( Show (..),
  )
import qualified Prelude as P
  ( undefined,
  )

undefined :: a
undefined = P.undefined
{-# WARNING undefined "`undefined` remains in code" #-}
