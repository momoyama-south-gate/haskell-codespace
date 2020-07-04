{-# LANGUAGE ScopedTypeVariables #-}

module My.Control.Applicative
  ( module X,
    Alternative (..),
    Applicative (..),
    prop_Applicative_Id,
    prop_Applicative_Comp,
    prop_Applicative_Homo,
    prop_Applicative_Inter,
    prop_Alternative_Left_Id,
    prop_Alternative_Right_Id,
    prop_Alternative_Assoc,
    liftA,
    liftA2,
    liftA3,
    many,
    some,
    (<*),
    (*>),
    (<**>),
  )
where

import qualified Prelude as P
import Control.Applicative as X (ZipList (..))
import My.Data.Function
import My.Data.Functor
import My.Data.Proxy
import My.Prelude.Internal
import My.Test.Arbitrary

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

infixl 4 <*>

-- | Applicative 法则

-- | 同一律（identity）
-- pure identity <*> v 等于 v
prop_Applicative_Id :: forall f a. (Applicative f, Eq (f a)) => f a -> Bool
prop_Applicative_Id fa = (pure identity <*> fa) == fa

-- | 组合律（composition）
-- pure (.) <*> u <*> v <*> w 等于 u <*> (v <*> w)
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
-- pure f <*> pure x 等于 pure (f x)
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

liftA :: Applicative f => (a -> b) -> f a -> f b
liftA fab apa = pure fab <*> apa

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 fabc apa apb = liftA fabc apa <*> apb 

liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 fabcd apa apb apc = liftA2 fabcd apa apb <*> apc

(<*) :: Applicative f => f a -> f b -> f a
(<*) = liftA2 const 

infixl 4 <*

(*>) :: Applicative f => f a -> f b -> f b
(*>) = flip (<*)

infixl 4 *>

(<**>) :: Applicative f => f a -> f (a -> b) -> f b
(<**>) = flip (<*>)

infixl 4 <**>

instance Applicative ((->) a) where
  -- pure :: b -> (a -> b)
  pure = const
  -- (<*>) :: (a -> b -> c) -> (a -> b) -> (a -> c)
  (fabc <*> fab) a = (fabc a) (fab a)


class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a

infixl 3 <|>

-- | Alternative 法则

-- | 左同一律（left identity）
-- empty <|> u 等于 u
prop_Alternative_Left_Id :: forall f a. (Alternative f, Eq (f a)) => f a -> Bool
prop_Alternative_Left_Id u = (empty <|> u) == u

-- | 右同一律（right identity）
-- u <|> empty 等于 u
prop_Alternative_Right_Id :: forall f a. (Alternative f, Eq (f a)) => f a -> Bool
prop_Alternative_Right_Id u = (u <|> empty) == u

-- | 结合律（associativity）
-- u <|> (v <|> w) 等于 (u <|> v) <|> w
prop_Alternative_Assoc ::
  forall f a.
  (Alternative f, Eq (f a)) =>
  f a ->
  f a ->
  f a ->
  Bool
prop_Alternative_Assoc u v w = left == right
  where
    left = u <|> (v <|> w)
    right = (u <|> v) <|> w

some :: Alternative f => f a -> f [a]
some x = liftA2 (:) x (many x)

many :: Alternative f => f a -> f [a]
many x = some x <|> pure []

-- --|
-- prop> prop_Alternative_Left_Id @ZipList
-- prop> prop_Alternative_Right_Id @ZipList
-- prop> prop_Alternative_Assoc @ZipList
instance Alternative ZipList where
  -- empty :: ZipList a
  empty = ZipList []
  -- (<|>) :: ZipList a -> ZipList a -> ZipList a
  xs <|> ys = case xs of
    ZipList [] -> ys
    _ -> xs

-- --|
-- prop> prop_Functor_Comp @ZipList
-- prop> prop_Functor_Id @ZipList
instance Functor ZipList where
  -- fmap :: (a -> b) -> ZipList a -> ZipList b
  fmap = zipMap

zipMap :: (a -> b) -> ZipList a -> ZipList b
zipMap f (ZipList xs) = ZipList (reverse $ go f xs []) where
  go f xs acc = case xs of 
    [] -> acc
    x:xs -> go f xs ((f x):acc)

reverse :: [a] -> [a]
reverse xs = go xs [] where
  go xs acc = case xs of
    [] -> acc
    x:xs -> go xs (x:acc)

-- -- |
-- prop> prop_Applicative_Id @ZipList
-- prop> prop_Applicative_Comp @ZipList
-- -- prop> prop_Applicative_Homo @ZipList Proxy
-- prop> prop_Applicative_Inter @ZipList 
instance Applicative ZipList where
  -- pure :: a -> ZipList a
  pure x = ZipList $ P.repeat x
  -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
  ZipList fs <*> ZipList x = ZipList (map uncurry1 ts) where
    ts = zip fs x
    
-- |
-- >>> zip [1,2,3] (repeat 1)
-- [(1,1),(2,1),(3,1)]
repeat :: a -> [a]
repeat x = xs where xs = x:xs

zip :: [a] -> [b] -> [(a, b)]
zip xs ys = reverse $ go xs ys [] where
  go [] _ acc = acc
  go _ [] acc = acc
  go (x:xs) (y:ys) acc = go xs ys ((x,y):acc)

uncurry1 :: ((a -> b), a) -> b
uncurry1 (f, x) = f x

map :: (a -> b) -> [a] -> [b]
map f xs = reverse $ go f xs [] where 
  go f xs acc = case xs of
    [] -> acc
    x:xs -> go f xs ((f x):acc)
  ZipList [] <*> _ = ZipList []
  _ <*> ZipList [] = ZipList []
  ZipList (f:fs) <*> ZipList (x:xs) = ZipList ((f x): getZipList (ZipList fs <*> ZipList xs))
    
