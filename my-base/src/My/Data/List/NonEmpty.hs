module My.Data.List.NonEmpty
  ( module X,
  )
where

import Data.List.NonEmpty as X (NonEmpty (..))
import My.Control.Applicative
import My.Control.Monad
import My.Data.Functor
import My.Data.Semigroup
import My.Prelude.Internal

-- data NonEmpty a = a :| [a]
-- infixr 5 :|

instance Semigroup (NonEmpty a)

instance Functor NonEmpty

instance Applicative NonEmpty

instance Monad NonEmpty

-- | 追加练习
-- 访问以下 url：
-- https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-List-NonEmpty.html
-- 从中选 10 个你认为常用的函数实现一下，不要忘记 export 。
-- （两个人各写 10 个不同的加起来就是 20 个）
