module My.Data.List.NonEmpty
  ( module X,
  )
where

import Data.List.NonEmpty as X (NonEmpty (..))
import My.Control.Applicative
import My.Control.Monad
import My.Data.Functor
import My.Data.List((++))
import My.Data.Semigroup
import My.Prelude.Internal

-- data NonEmpty a = a :| [a]
-- infixr 5 :|

-- |
-- NO NonEmpty: prop> prop_Semigroup_Assoc @(NonEmpty Int)
instance Semigroup (NonEmpty a) where
  -- <> :: NonEmpty a -> NonEmpty a -> NonEmpty a
  (x :| xs) <> (y :| ys) = x :| (xs ++ (y:ys))

instance Functor NonEmpty where
  -- fmap :: (a -> b) -> NonEmpty a -> NonEmpty b
  fmap = map

instance Applicative NonEmpty where
  -- pure :: a -> NonEmpty a
  pure x = x :| []
  -- <*> :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
  (f :| []) <*> xs = fmap f xs
  (f :| (f2:fs)) <*> nxs = fmap f nxs <> ((f2 :| fs) <*> nxs)

(<|) :: a -> NonEmpty a -> NonEmpty a 
infixr 5 <|
(<|) x (y :| ys) = x :| (y:ys)

map :: (a -> b) -> NonEmpty a -> NonEmpty b
map f (x :| xs) = (f x) :| (fmap f xs)

instance Monad NonEmpty

-- | 追加练习
-- 访问以下 url：
-- https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-List-NonEmpty.html
-- 从中选 10 个你认为常用的函数实现一下，不要忘记 export 。
-- （两个人各写 10 个不同的加起来就是 20 个）
