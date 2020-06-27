module My.Data.List where

import My.Control.Applicative
import My.Control.Monad
import My.Data.Functor
import My.Data.Maybe
import My.Data.Monoid
import My.Data.Semigroup
import My.Data.Tuple
import My.Prelude.Internal
import My.Data.Function

instance Semigroup [a] where
  (<>) = (++)

instance Monoid [a] where
  mempty = []

instance Functor [] where
  fmap = map

instance Applicative [] where
  pure a = [a]
  (<*>) fab fa =
    case (fab, fa) of
      ([], _) -> []
      (_, []) -> []
      (f: tf, _) -> map f fa ++ (tf <*> fa)

instance Alternative []

instance Monad []

(++) :: [a] -> [a] -> [a]
(++) l1 l2 =
  case (l1, l2) of
    ([], _) -> l2
    (h1: t1, _) -> h1 : (t1 ++ l2)

infixr 5 ++

head :: [a] -> Maybe a
head l =
  case l of
    [] -> Nothing
    a : _ -> Just a

last :: [a] -> Maybe a
last l =
  case l of
    [] -> Nothing
    a : [] -> Just a
    _: tl -> last tl

tail :: [a] -> Maybe [a]
tail [] = Nothing
tail (_ : tl) = Just tl

init :: [a] -> Maybe [a]
init [] = Nothing
inti (hd : tl) = Just (hd : fromMaybe [] (init tl))

null :: [a] -> Bool
null [ ]= False
null _ = True

length :: [a] -> Int
length [] = 0
length (_ : tl) = 1 + length tl

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (a : tl) = f a : map f tl

reverse :: [a] -> [a]
reverse [] = []
reverse (hd : tl) = reverse tl ++ [hd]

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ o [] = o
foldl f o (hd : tl) = foldl f (f o hd) tl

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ o [] = o
foldr f o (hd : tl) = f hd (foldr f o tl)

-- |
-- >>> take 10 []
-- []
-- >>> take 10 [1,2,3]
-- [1,2,3]
-- >>> take 1 [1,2,4]
-- [1]
-- >>> take 2 [1,2,4]
-- [1,2]
take :: Int -> [a] -> [a]
take n l = run n l
  where
    run :: Int -> [a] -> [a]
    run 0 _ = []
    run _ [] = []
    run n (hd: tl) = hd : run (n-1) tl

-- |
-- >>> drop 10 []
-- []
-- >>> drop 10 [1,2,3]
-- []
-- >>> drop 1 [1,2,4]
-- [2,4]
-- >>> drop 2 [1,2,4]
-- [4]
drop :: Int -> [a] -> [a]
drop _ [] = []
drop 0 l = l
drop n (_ : tl) = drop (n-1) tl

-- |
-- >>> span (>0) []
-- ([],[])
-- >>> span (>0) [1]
-- ([1],[])
-- >>> span (>0) [1,-2]
-- ([1],[-2])
span :: (a -> Bool) -> [a] -> ([a], [a])
span f [] = ([], [])
span f (hd : tl) = 
  if f hd 
  then (hd : fst other, snd other)
  else (fst other, hd : snd other)
     where
       other = span f tl

break :: (a -> Bool) -> [a] -> ([a], [a])
break f l = swap $ span f l

-- |
-- >>> elem 1 []
-- False
-- >>> elem 1 [2]
-- False
-- >>> elem 1 [2,1]
-- True
elem :: (Eq a) => a -> [a] -> Bool
elem _ [] = False
elem a (hd : tl) = 
  if hd == a 
  then True 
  else elem a tl

lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup _ [] = Nothing
lookup a (hd : tl) = 
  if a == fst hd 
  then Just $ snd hd 
  else lookup a tl

find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find f (hd : tl) =
  if f hd
  then Just hd
  else find f tl

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (hd : tl) =
  if f hd
  then hd : other
  else other
    where
      other = filter f tl

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition = span

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
