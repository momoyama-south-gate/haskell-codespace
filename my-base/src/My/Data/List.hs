module My.Data.List where

import My.Control.Applicative
import My.Control.Monad
import My.Data.Functor
import My.Data.Maybe
import My.Data.Monoid
import My.Data.Semigroup
import My.Prelude.Internal

instance Semigroup [a]

instance Monoid [a]

instance Functor []

instance Applicative []

instance Alternative []

instance Monad []

(++) :: [a] -> [a] -> [a]
(++) = undefined

infixr 5 ++

head :: [a] -> Maybe a
head = undefined

last :: [a] -> Maybe a
last = undefined

tail :: [a] -> Maybe [a]
tail = undefined

init :: [a] -> Maybe [a]
init = undefined

null :: [a] -> Bool
null = undefined

length :: [a] -> Int
length = undefined

map :: (a -> b) -> [a] -> [b]
map = undefined

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
