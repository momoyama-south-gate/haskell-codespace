{-# LANGUAGE ScopedTypeVariables #-}

module My.Control.Monad where

import My.Data.Function
import My.Data.Functor
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
return = pure

(>>) :: Monad m => m a -> m b -> m b
(>>) ma mb = ma >>= const mb
-- <>:: (a->b) -> ma -> mb
-- (>>=)::m a -> (a -> m b) -> m b
-- const:: Const a -> a
-- (<*>)::f(a->b)-> f a -> f b

infixl 1 >>

join :: Monad m => m (m a) -> m a
join mma = mma >>= identity
-- (>>=)::m (m a) -> (m a -> m a) -> m a

(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip (>>=)

infixr 1 =<<

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
(>=>) fa fb = ((=<<) fb) . fa
-- b -> m c
-- (>>=)::m b -> (b -> m c) -> m c

infixr 1 >=>

(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
(<=<) = flip (>=>)

infixr 1 <=<

forever :: Applicative f => f a -> f b
forever fa = fb
  where
    fb = fa *> fb

-- 不看标准库写不出来啊。。。。
-- liftA2::(Bool -> [a] -> [a]) -> f Bool -> f[a] -> f[a]
filterM :: Applicative f => (a -> f Bool) -> [a] -> f [a]
filterM f = foldl (\fl x -> liftA2 (\y l-> if y then x:l else l) (f x) fl) (pure [])
  where
    foldl :: (b -> a -> b) -> b -> [a] -> b
    foldl _ o [] = o
    foldl f o (hd : tl) = foldl f (f o hd) tl

-- liftA2::(a->b->c)->f a->f b->f c
-- liftA2::((b,c)->([b],[c])->([b],[c]))-> f(b,c) -> f ([b],[c]) -> f([b],[c])
mapAndUnzipM :: Applicative f => (a -> f (b, c)) -> [a] -> f ([b], [c])
mapAndUnzipM f = foldl (\fl x -> liftA2 (\(b,c) (lf,lr)-> (b:lf, c:lr)) (f x) fl) (pure ([],[]))
  where
    foldl :: (b -> a -> b) -> b -> [a] -> b
    foldl _ o [] = o
    foldl f o (hd : tl) = foldl f (f o hd) tl

-- liftA2::(c -> [c] ->[c])-> f c -> f [c] -> f [c]
zipWithM :: Applicative f => (a -> b -> f c) -> [a] -> [b] -> f [c]
zipWithM f la lb = foldl (\fl (a,b) -> liftA2 (\y l -> y : l) (f a b) fl) (pure ([])) (zip la lb)
  where
    foldl :: (b -> a -> b) -> b -> [a] -> b
    foldl _ o [] = o
    foldl f o (hd : tl) = foldl f (f o hd) tl

    zip :: [a] -> [b] -> [(a, b)]
    zip _ [] = []
    zip [] _ = []
    zip (a : tla) (b: tlb) = (a, b) : zip tla tlb   

zipWithM_ :: Applicative f => (a -> b -> f c) -> [a] -> [b] -> f ()
zipWithM_ f la lb  = const (pure ()) (zipWithM f la lb)

replicateM :: Applicative f => Int -> f a -> f [a]
replicateM 0 _ = pure []
replicateM n f = liftA2 (\a l -> a : l) f (replicateM (n-1) f)

replicateM_ :: Applicative f => Int -> f a -> f ()
replicateM_ n f = const (pure ()) (replicateM_ n f)

guard :: Alternative f => Bool -> f ()
guard True = pure ()
guard False = empty

when :: Applicative f => Bool -> f () -> f ()
when True f = f
when False _ = pure ()

unless :: Applicative f => Bool -> f () -> f ()
unless False f = f
unless True _ = pure ()

instance Monad ((->) r) where
  (>>=) ma fa = \r -> (fa $ ma r) r
-- (r->a) -> (a -> (r->b)) -> (r-> b)

