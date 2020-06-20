module My.Data.Void
  ( Void,
    absurd,
  )
where

import My.Prelude.Internal

data Void = Void Void

absurd :: Void -> a
absurd (Void void) = absurd void
