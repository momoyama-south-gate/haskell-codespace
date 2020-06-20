module My.Data.Void
  ( Void,
    absurd,
  )
where

import My.Prelude.Internal

data Void = Void Void-- TODO: 完成 Void 的定义

absurd :: Void -> a
absurd (Void void) = absurd void
