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
import My.Data.List

-- data NonEmpty a = a :| [a]
-- infixr 5 :|

instance Semigroup (NonEmpty a) where
  (<>) (a :| ta) ( b :| tb) = a :| (ta ++ (b : tb)) 

instance Functor NonEmpty where
  fmap f (a :| ta) = (f a) :| (fmap f ta) 

instance Applicative NonEmpty where
  pure = (\a -> (a :| [a]))
  (<*>) nlf nla =
    case (nlf, nla) of
      (f :| _, a :| []) -> (f a :| [])
      (f :| [], a :| _) -> (f a :| [])
      (f :| tf, a :| ta) -> (f a :| (tf <*> ta))

instance Monad NonEmpty

-- | 追加练习
-- 访问以下 url：
-- https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-List-NonEmpty.html
-- 从中选 10 个你认为常用的函数实现一下，不要忘记 export 。
-- （两个人各写 10 个不同的加起来就是 20 个）
