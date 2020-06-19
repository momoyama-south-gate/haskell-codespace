{-# LANGUAGE ScopedTypeVariables #-}

module My.Test.Tutorial
  (
  )
where

-- 为了方便，在这个 module 我们用标准库的 Prelude

import My.Data.Proxy
import My.Test.Arbitrary
import Prelude

-- | doctest 的使用方法
-- >>> 1
-- 1
-- >>> True
-- True

-- | 比较的是 ghci 里打印出来的结果，所以
-- >>> [1, 2, 3]
-- [1,2,3]

-- | 而
-- （把这部分删掉试试）>>> [1, 2, 3]
-- [1, 2, 3]

-- | 感觉测试没被更新的时候就 `make clean` 一下

-- | 在 doctest 里使用 QuickCheck
-- 返回值必须是 Bool
-- True 的时候成功，False的时候失败
-- prop> True
-- (把这部分删掉试试) prop> 1 == 2

-- | 参数多态的时候，QuickCheck 会自动帮你生成测试数据，比如说下面的 `xs` 和 `ys` 都是自动生成的
-- prop> reverse (xs <> ys) == reverse ys ++ reverse xs

-- | 当你把测试写成最后返回 Bool 的函数的时候，甚至连 `xs` `ys` 都不用写
-- prop> prop_Reverse_App
prop_Reverse_App :: Eq a => [a] -> [a] -> Bool
prop_Reverse_App xs ys = reverse (xs <> ys) == reverse ys ++ reverse xs

-- | 特设多态的时候，QuickCheck 不知道你想要测试的是哪个 instance
-- （把这部分删掉试试）prop> fmap id xs == xs

-- | 因此我们可以通过 -XTypeApplications（已经帮你设定好了）来给 QuickCheck 一点提示
-- 由于：
-- fmap :: forall f a b. (a -> b) -> f a -> f b
-- 注意类型参数出现的顺序
-- 所以：
-- fmap @[] :: (a -> b) -> [a] -> [b]
-- fmap @[] @Int :: (Int -> b) -> [Int] -> [b]
-- 这里由于 `id` 是参数多态的，只要让 QuickCheck 知道我们要 `[]` 的 `fmap` 就可以顺利测试了
-- prop> fmap @[] id xs == xs
-- 同样的
-- prop> fmap @Maybe id xs == xs

-- | 下面来简单介绍一下怎样用 QuickCheck 来测试类型类的法则

-- | Semigroup 法则（摘自 ../Data/Semigroup.hs）

-- | 结合律（associativity）
-- a <> (b <> c) 等于 (a <> b) <> c
-- prop> prop_Semigroup_Assoc @String
prop_Semigroup_Assoc :: forall a. (Eq a, Semigroup a) => a -> a -> a -> Bool
prop_Semigroup_Assoc a b c = (a <> b) <> c == a <> (b <> c)

-- | Monoid 法则（摘自 ../Data/Monoid.hs）

-- | 左同一律（left identity）
-- mempty <> a 等于 a
-- prop> prop_Monoid_LeftId @(Maybe String)
prop_Monoid_LeftId :: forall a. (Eq a, Monoid a) => a -> Bool
prop_Monoid_LeftId a = mempty <> a == a

-- | 右同一律（right identity）
-- a <> mempty 等于 a
-- prop> prop_Monoid_RightId @(String, String)
prop_Monoid_RightId :: forall a. (Eq a, Monoid a) => a -> Bool
prop_Monoid_RightId a = a <> mempty == a

-- | Functor 法则（摘自 ../Data/Functor.hs）

-- | 组成律（composition）
-- fmap (g . f) 等于 (fmap g) . (fmap f
-- prop> prop_Functor_Comp @[]
prop_Functor_Comp ::
  forall f a b c.
  (Functor f, Eq (f c)) =>
  Fun a b ->
  Fun b c ->
  f a ->
  Bool
prop_Functor_Comp fab fbc fa = fmap (g . f) fa == ((fmap g) . (fmap f)) fa
  where
    f :: a -> b
    f = applyFun fab
    g :: b -> c
    g = applyFun fbc

-- | 同一律（identity）
-- fmap id 等于 id
-- prop> prop_Functor_Id @Maybe
prop_Functor_Id :: forall f a. (Functor f, Eq (f a)) => f a -> Bool
prop_Functor_Id fa = fmap id fa == id fa

-- | Applicative 法则（摘自 ../Control.Applicative.hs）

-- | 同一律（identity）
-- pure id <*> v 等于 v
-- prop> prop_Applicative_Id @[]
prop_Applicative_Id :: forall f a. (Applicative f, Eq (f a)) => f a -> Bool
prop_Applicative_Id fa = (pure id <*> fa) == fa

-- | 组合律（composition）
-- pure (.) <*> u <*> v <*> w 等于 u <*> (v <*> w)
-- prop> prop_Applicative_Comp @Maybe
prop_Applicative_Comp ::
  forall f a b c.
  (Applicative f, Eq (f c)) =>
  f (Fun a b) ->
  f (Fun b c) ->
  f a ->
  Bool
prop_Applicative_Comp fab fbc w = (pure (.) <*> u <*> v <*> w) == (u <*> (v <*> w))
  where
    v :: f (a -> b)
    v = applyFun <$> fab
    u :: f (b -> c)
    u = applyFun <$> fbc

-- | 同态（homomorphism）
-- 同态的实现稍微有点特殊，必须手动给个 Proxy
-- Proxy 在 My.Data.Proxy 里有定义，直接拿来用就好了
-- pure f <*> pure x 等于 pure (f x)
-- prop> prop_Applicative_Homo @(Either ()) Proxy
prop_Applicative_Homo ::
  forall f a b.
  (Applicative f, Eq (f b)) =>
  Proxy f ->
  Fun a b ->
  a ->
  Bool
prop_Applicative_Homo _ fab x = left == right
  where
    ab :: a -> b
    ab = applyFun fab
    left :: f b
    left = pure ab <*> pure x
    right :: f b
    right = pure (ab x)

-- | 互换（interchange）
-- u <*> pure y 等于 pure ($ y) <*> u
-- prop> prop_Applicative_Inter @(Either String)
prop_Applicative_Inter ::
  forall f a b.
  (Applicative f, Eq (f b)) =>
  f (Fun a b) ->
  a ->
  Bool
prop_Applicative_Inter fab y = (u <*> pure y) == (pure ($ y) <*> u)
  where
    u :: f (a -> b)
    u = applyFun <$> fab
