module My.Test.Arbitrary
  ( module X,
  )
where

import GHC.Generics as X
  ( Generic,
  )
import Test.QuickCheck as X
  ( Arbitrary (..),
    Fun (..),
    Function (..),
    applyFun,
    applyFun2,
    applyFun3,
  )
