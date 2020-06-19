module My.Data.Void
  ( Void,
    absurd,
  )
where

import My.Prelude.Internal

data Void -- TODO: 完成 Void 的定义

absurd :: Void -> a
absurd = undefined
