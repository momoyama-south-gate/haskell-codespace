module My.Data.List where

import My.Control.Applicative
import My.Data.Functor
import My.Data.Function
import My.Data.Tuple
import My.Data.Proxy
import Data.Maybe (Maybe(..))
import My.Data.Monoid
import My.Data.Semigroup
import My.Prelude.Internal

-- |
-- prop> prop_Functor_Comp @([])
-- prop> prop_Semigroup_Assoc @([] (Sum Int))
-- prop> prop_Monoid_LeftId @([] (Sum Int))
-- prop> prop_Monoid_RightId @([] (Sum Int))
-- prop> prop_Functor_Id @[]
-- prop> prop_Functor_Comp @([])
-- prop> prop_Applicative_Id @[]
-- prop> prop_Applicative_Comp @[]
-- prop> prop_Applicative_Homo @[] Proxy
-- FAILE prop> prop_Applicative_Inter @[] 
instance Semigroup [a] where
  -- <> :: [a] -> [a] -> [a]
  (<>) = (++)

instance Monoid [a] where
  -- mempty :: [a]
  mempty = []

instance Functor [] where
  -- fmap :: (a -> b) -> [a] -> [b]
  fmap = map

instance Applicative [] where
  -- pure :: a -> [a]
  pure x = [x]
  -- <*> :: [a -> b] -> [a] -> [b]
  [] <*> _ = []
  _ <*> [] = []
  (f:_) <*> xs = fmap f xs

(++) :: [a] -> [a] -> [a]
(++) x ys = case x of
  [] -> ys
  x:xs -> x:(xs ++ ys)

infixr 5 ++

head :: [a] -> Maybe a
head [] = Nothing
head (x:_) = Just x

last :: [a] -> Maybe a
last [] = Nothing
last [x] = Just x
last (x:xs) = last xs

tail :: [a] -> Maybe [a]
tail [] = Nothing
tail (_:xs) = Just xs

-- |
-- -- prop> init [] == Nothing
-- -- prop> init [1] == Just []
-- -- prop> init [2,1] == Just [2]
-- -- prop> init [2,1,3,4] == Just [2,1,3]
init :: [a] -> Maybe [a]
init [] = Nothing
init xs = Just (reverse $ go xs []) where
  go xs acc = case xs of
    [x] -> acc
    x:xs -> go xs (x:acc)


null :: [a] -> Bool
null [] = True
null _ = False

length :: [a] -> Int
length xs = go xs 0 where
  go xs acc = case xs of
    [] -> acc
    _:xs -> go xs (acc+1)

map :: (a -> b) -> [a] -> [b]
map f xs = reverse $ go f xs [] where 
  go f xs acc = case xs of
    [] -> acc
    x:xs -> go f xs ((f x):acc)

reverse :: [a] -> [a]
reverse xs = go xs [] where
  go xs acc = case xs of
    [] -> acc
    x:xs -> go xs (x:acc)

-- --|
-- -- prop> foldl (-) 10 [1,2,3] == 4
-- -- prop> foldl (-) 10 [1,2,3,4] == 0
-- -- prop> foldl (-) 0 [1,2,3,4] == -10
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f n xs = case xs of
  [] -> n
  x:xs -> foldl f (f n x) xs

-- --|
-- >>> foldr (-) 10 [1,2,3]
-- -8
-- >>> foldr (-) 0 [1,2,3,4]
-- -2
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f n xs = case xs of
  [] -> n
  x:xs -> f x (foldr f n xs)

-- --|
-- prop> take 5 "Hello World!" == "Hello"
-- prop> take 3 [1,2,3,4,5] == [1,2,3]
-- prop> take 3 [1,2] == [1,2]
-- prop> take 3 [] == []
-- prop> take (-1) [1,2] == []
-- prop> take 0 [1,2] == []
take :: Int -> [a] -> [a]
take x ys = reverse $ go x ys [] where
  go 0 _ acc = acc
  go _ [] acc = acc
  go x (y:ys) acc = if x < 0 then [] else go (x-1) ys (y:acc)

-- --|
-- prop> drop 6 "Hello World!" == "World!"
-- prop> drop 3 [1,2,3,4,5] == [4,5]
-- prop> drop 3 [1,2] == []
-- prop> drop 3 [] == []
-- prop> drop (-1) [1,2] == [1,2]
-- prop> drop 0 [1,2] == [1,2]
drop :: Int -> [a] -> [a]
drop _ [] = []
drop 0 ys = ys
drop x (y:ys) = if x < 0 then (y:ys) else drop (x-1) ys

-- --|
-- prop> span (< 3) [1,2,3,4,1,2,3,4] == ([1,2],[3,4,1,2,3,4])
-- prop> span (< 9) [1,2,3] == ([1,2,3],[])
-- prop> span (< 0) [1,2,3] == ([],[1,2,3])
span :: (a -> Bool) -> [a] -> ([a], [a])
span f xs = (reverse a, b) where
  (a,b) = go f xs ([],[])
  go f xs (ac1,ac2) = case xs of
    [] -> (ac1,[])
    x:xs -> if f x then go f xs (x:ac1,[]) else (ac1,x:xs) 

-- --|
-- prop> break (> 3) [1,2,3,4,1,2,3,4] == ([1,2,3],[4,1,2,3,4])
-- prop> break (< 9) [1,2,3] == ([],[1,2,3])
-- prop> break (> 9) [1,2,3] == ([1,2,3],[])
break :: (a -> Bool) -> [a] -> ([a], [a])
break f xs = (reverse a, b) where
  (a, b) = go f xs ([],[])
  go f xs (ac1, ac2) = case xs of
    [] -> (ac1,[])
    x:xs -> if f x then (ac1,x:xs) else go f xs (x:ac1,[])

-- --|
-- prop> elem 1 [] == False
-- prop> elem 1 [2,3] == False
-- prop> elem 1 [2,1,3] == True
elem :: (Eq a) => a -> [a] -> Bool
elem n ys = foldl (\b x -> b || (x == n)) False ys

-- --|
-- prop> lookup 2 [] == Nothing
-- prop> lookup 2 [(1,2),(3,2)] == Nothing
-- prop> lookup 2 [(1,2),(2,4),(3,2)] == Just 4
lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup _ [] = Nothing
lookup n ((a,b):xs) = if n == a then Just b else lookup n xs

-- --|
-- prop> find (>2) [] == Nothing
-- prop> find (>2) [1,2] == Nothing
-- prop> find (>2) [1,3,4,5] == Just 3
find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find f (x:xs) = if f x then Just x else find f xs

-- -- |
-- prop> filter (>2) [] == []
-- prop> filter (>2) [1,2] == []
-- prop> filter (>2) [1,3,4,5] == [3,4,5]
filter :: (a -> Bool) -> [a] -> [a]
filter f xs = reverse $ go f xs [] where
  go f xs acc = case xs of
    [] -> acc
    x:xs -> if f x then go f xs (x:acc) else go f xs acc

-- -- |
-- prop> partition (>2) [] == ([],[])
-- prop> partition (>2) [1,2] == ([],[1,2])
-- prop> partition (>2) [1,3,4,5] == ([3,4,5],[1])
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition f xs = (filter f xs, filter (not . f) xs)

-- --|
-- prop> [] !! 3 == Nothing
-- prop> [1,2] !! 5 == Nothing
-- prop> [1,2,3] !! 2 == Just 3
-- prop> [1,2,3] !! 1 == Just 2
(!!) :: [a] -> Int -> Maybe a
(!!) [] _ = Nothing
(!!) (x:_) 0 = Just x
(!!) (_:xs) n = (!!) xs (n-1)

infixl 9 !!

-- --|
-- prop> zip [1, 2] ['a', 'b'] == [(1, 'a'), (2, 'b')]
-- prop> zip [1] ['a', 'b'] == [(1, 'a')]
-- prop> zip [1, 2] ['a'] == [(1, 'a')]
zip :: [a] -> [b] -> [(a, b)]
zip xs ys = reverse $ go xs ys [] where
  go [] _ acc = acc
  go _ [] acc = acc
  go (x:xs) (y:ys) acc = go xs ys ((x,y):acc)

-- |
-- >>> zipWith (+) [1, 2, 3] [4, 5, 6]
-- [5,7,9]
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f xs ys = 
  map (uncurry f) (zip xs ys)

-- --|
-- prop> unzip [] == ([],[])
-- prop> unzip [(1,2),(2,3)] == ([1,2],[2,3])
unzip :: [(a, b)] -> ([a], [b])
unzip xs = (reverse as, reverse bs) where
  (as, bs) = go xs ([],[])
  go xs (ac1,ac2) = case xs of
    [] -> (ac1,ac2)
    (a,b):xs -> go xs (a:ac1, b:ac2)

-- |
-- >>> lines ""
-- []
-- >>> lines "\n"
-- [""]
-- >>> lines "one"
-- ["one"]
-- >>> lines "one\n"
-- ["one"]
-- >>> lines "one\n\n"
-- ["one",""]
-- >>> lines "one\ntwo"
-- ["one","two"]
-- >>> lines "one\ntwo\n"
-- ["one","two"]
lines :: String -> [String]
lines xs = reverse $ go xs [] [] where
  go xs word acc = case xs of
    [] -> if word == [] then acc else (reverse word:acc)
    x:xs -> if x == '\n' then go xs [] (reverse word:acc) else go xs (x:word) acc

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
