{-# LANGUAGE ScopedTypeVariables #-}

module My.Control.Monad where

import My.Control.Applicative
import My.Data.Proxy
import My.Data.Function
import My.Data.Functor
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
return = pure

(>>) :: Monad m => m a -> m b -> m b
(>>) ma mb = ma >>= const mb

infixl 1 >>

join :: Monad m => m (m a) -> m a
join ma = ma >>= identity

(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip (>>=)

infixr 1 =<<

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
(>=>) f g a = f a >>= g

infixr 1 >=>

(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
(<=<) = flip (>=>)

infixr 1 <=<

forever :: Applicative f => f a -> f b
forever fa = fa *> forever fa

filterM :: Applicative f => (a -> f Bool) -> [a] -> f [a]
filterM _ [] = pure []
filterM f (x:xs) = liftA2 (\flg -> if flg then (x:) else identity) (f x) (filterM f xs)

mapAndUnzipM :: Applicative f => (a -> f (b, c)) -> [a] -> f ([b], [c])
mapAndUnzipM _ [] = pure ([], [])
mapAndUnzipM f (x:xs) = liftA2 (\(b,c) (bs,cs) -> ((b:bs), (c:cs)) ) (f x) (mapAndUnzipM f xs)

zipWithM :: Applicative f => (a -> b -> f c) -> [a] -> [b] -> f [c]
zipWithM _ [] _ = pure []
zipWithM _ _ [] = pure []
zipWithM f (x:xs) (y:ys) = liftA2 (:) (f x y) (zipWithM f xs ys)

zipWithM_ :: Applicative f => (a -> b -> f c) -> [a] -> [b] -> f ()
zipWithM_ f la lb = void $ zipWithM f la lb

replicateM :: Applicative f => Int -> f a -> f [a]
replicateM n fa = if n <= 0 then pure [] else
  liftA2 (:) fa (replicateM (n-1) fa)

replicateM_ :: Applicative f => Int -> f a -> f ()
replicateM_ n fa = void $ replicateM n fa

guard :: Alternative f => Bool -> f ()
guard True = pure ()
guard False = empty

when :: Applicative f => Bool -> f () -> f ()
when True fx = fx
when _ _ = pure ()

unless :: Applicative f => Bool -> f () -> f ()
unless False fx = fx
unless _ _ = pure ()

instance Monad ((->) r) where
  -- >>= :: (r -> a) -> (a -> r -> b) -> r -> b
  (fra >>= farb) r = farb (fra r) r

