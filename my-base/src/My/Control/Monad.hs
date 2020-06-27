{-# LANGUAGE ScopedTypeVariables #-}

module My.Control.Monad where

import My.Control.Applicative
import My.Data.Proxy
import My.Prelude.Internal
import My.Test.Arbitrary

class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b

infixl 1 >>=

-- | 结合律（associativity）
-- (m >>= f) >>= g 等于 m >>= (\x -> f x >>= g)
prop_Monad_Assoc ::
  forall m a b c.
  (Monad m, Eq (m c)) =>
  m a ->
  Fun a (m b) ->
  Fun b (m c) ->
  Bool
prop_Monad_Assoc ma amb bmc = left == right
  where
    f :: a -> m b
    f = applyFun amb
    g :: b -> m c
    g = applyFun bmc
    left :: m c
    left = (ma >>= f) >>= g
    right :: m c
    right = ma >>= (\x -> f x >>= g)

-- | 左同一律（left identity）
-- pure a >>= f 等于 f a
prop_Monad_Left_Id ::
  forall m a b.
  (Monad m, Eq (m b)) =>
  a ->
  Fun a (m b) ->
  Bool
prop_Monad_Left_Id a amb = (pure a >>= f) == f a
  where
    f :: a -> m b
    f = applyFun amb

-- | 右同一律（right identity）
-- m >>= pure 等于 m
prop_Monad_Right_Id ::
  forall m a.
  (Monad m, Eq (m a)) =>
  m a ->
  Bool
prop_Monad_Right_Id ma = (ma >>= pure) == ma

return :: Monad m => a -> m a
return = undefined

(>>) :: Monad m => m a -> m b -> m b
(>>) = undefined

infixl 1 >>

join :: Monad m => m (m a) -> m a
join = undefined

(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = undefined

infixr 1 =<<

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
(>=>) = undefined

infixr 1 >=>

(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
(<=<) = undefined

infixr 1 <=<

forever :: Applicative f => f a -> f b
forever = undefined

filterM :: Applicative f => (a -> f Bool) -> m a -> m a
filterM = undefined

filterM_ :: Applicative f => (a -> f Bool) -> f a -> f ()
filterM_ = undefined

mapAndUnzipM :: Applicative f => (a -> f (b, c)) -> [a] -> f ([b], [c])
mapAndUnzipM = undefined

zipWithM :: Applicative f => (a -> b -> f c) -> [a] -> [b] -> m [c]
zipWithM = undefined

zipWithM_ :: Applicative f => (a -> b -> f c) -> [a] -> [b] -> m ()
zipWithM_ = undefined

replicateM :: Applicative f => Int -> f a -> f [a]
replicateM = undefined

replicateM_ :: Applicative f => Int -> f a -> f ()
replicateM_ = undefined

guard :: Alternative f => Bool -> f ()
guard = undefined

when :: Applicative f => Bool -> f () -> f ()
when = undefined

unless :: Applicative f => Bool -> f () -> f ()
unless = undefined

instance Monad ((->) r)
