module My.Data.List where

import My.Control.Applicative
import My.Control.Monad
import My.Data.Functor
import My.Data.Maybe
import My.Data.Monoid
import My.Data.Semigroup
import My.Prelude.Internal

instance Semigroup [a] where
  (<>) = (++)

instance Monoid [a] where
  mempty = []

instance Functor [] where
  fmap = map

instance Applicative [] where
  pure = (\a -> [a])
  (<*>) fab fa =
    case (fab, fa) of
      ([], _) -> []
      (_, []) -> []
      (f: tf, a: ta) -> (f a) : (tf <*> ta)

instance Alternative []

instance Monad []

(++) :: [a] -> [a] -> [a]
(++) l1 l2 =
  case (l1, l2) of
    ([], _) -> l2
    (_, []) -> l1
    (h1: t1, _) -> h1 : (t1 ++ l2)

infixr 5 ++

head :: [a] -> Maybe a
head l =
  case l of
    [] -> Nothing
    a : t -> Just a

last :: [a] -> Maybe a
last l =
  case l of
    [] -> Nothing
    a : [] -> Just a
    a: tl -> last tl

tail :: [a] -> Maybe [a]
tail = undefined

init :: [a] -> Maybe [a]
init = undefined

null :: [a] -> Bool
null = undefined

length :: [a] -> Int
length = undefined

map :: (a -> b) -> [a] -> [b]
map f l =
  case l of
    [] -> []
    a : tl -> (f a) : (map f tl)

reverse :: [a] -> [a]
reverse = undefined

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl = undefined

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr = undefined

take :: Int -> [a] -> [a]
take = undefined

drop :: Int -> [a] -> [a]
drop = undefined

span :: (a -> Bool) -> [a] -> ([a], [a])
span = undefined

break :: (a -> Bool) -> [a] -> ([a], [a])
break = undefined

elem :: (Eq a) => a -> [a] -> Bool
elem = undefined

lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup = undefined

find :: (a -> Bool) -> [a] -> Maybe a
find = undefined

filter :: (a -> Bool) -> [a] -> [a]
filter = undefined

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition = undefined

(!!) :: [a] -> Int -> Maybe a
(!!) = undefined

infixl 9 !!

zip :: [a] -> [b] -> [(a, b)]
zip = undefined

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith = undefined

unzip :: [(a, b)] -> ([a], [b])
unzip = undefined

lines :: String -> [String]
lines = undefined

words :: String -> [String]
words = undefined

unlines :: [String] -> String
unlines = undefined

unwords :: [String] -> String
unwords = undefined

nub :: Eq a => [a] -> [a]
nub = undefined

delete :: Eq a => a -> [a] -> [a]
delete = undefined

(\\) :: Eq a => [a] -> [a] -> [a]
(\\) = undefined

infix 5 \\

union :: Eq a => [a] -> [a] -> [a]
union = undefined

intersect :: Eq a => [a] -> [a] -> [a]
intersect = undefined

sort :: Ord a => [a] -> [a]
sort = undefined

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy = undefined

-- | 追加练习
-- 访问以下 url：
-- http://hackage.haskell.org/package/base-4.14.0.0/docs/Data-List.html
-- 从中选 10 个你认为常用的函数（除去已实现的）实现一下。
-- （两个人各写 5 个不同的加起来就是 10 个）
