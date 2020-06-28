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
-- >>> span (\x -> x/='\n') "ads"
-- ("ads","")
-- >>> span (\x -> x/='\n') "ads\nasdff\nasdf"
-- ("ads","\nasdff\nasdf")
span :: (a -> Bool) -> [a] -> ([a], [a])
span f [] = ([], [])
span f (hd : tl) = 
  if f hd 
  then (hd : fst other, snd other)
  else ([], hd : tl)
     where
       other = span f tl
-- |
-- >>> break (>5) [1..10]
-- ([1,2,3,4,5],[6,7,8,9,10])
break :: (a -> Bool) -> [a] -> ([a], [a])
break f [] = ([], [])
break f (hd : tl) =
  if f hd
  then ([], hd : tl)
  else (hd : fst other, snd other)
    where
       other = break f tl

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

-- |
-- >>> [1,2,3] !! 0
-- Just 1
-- >>> [1,2,3] !! 2
-- Just 3
-- >>> [1,2,3] !! 3
-- Nothing
-- >>> [1,2,3] !! (-1)
-- Nothing
(!!) :: [a] -> Int -> Maybe a
(!!) [] _ = Nothing
(!!) (a : tl) n =
  if n < 0
  then Nothing
  else if n == 0
  then Just a
  else (!!) tl (n-1)

infixl 9 !!

-- |
-- >>> zip [] [1]
-- []
-- >>> zip [1] [1]
-- [(1,1)]
-- >>> zip [1,2] [1]
-- [(1,1)]
-- >>> zip [1] [1,2]
-- [(1,1)]
-- >>> zip [1] []
-- []
zip :: [a] -> [b] -> [(a, b)]
zip _ [] = []
zip [] _ = []
zip (a : tla) (b: tlb) = (a, b) : zip tla tlb 

-- |
-- >>> zipWith (+) [] [1]
-- []
-- >>> zipWith (+) [1] [1]
-- [2]
-- >>> zipWith (+) [1,2] [1]
-- [2]
-- >>> zipWith (+) [1] [1,2]
-- [2]
-- >>> zipWith (+) [1] []
-- []
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (a:tla) (b:tlb) = f a b : zipWith f tla tlb

-- |
-- >>> unzip []
-- ([],[])
-- >>> unzip [(1,2)]
-- ([1],[2])
unzip :: [(a, b)] -> ([a], [b])
unzip [] = ([], [])
unzip ((a, b) : tl) = (a : fst other, b : snd other)
  where
    other = unzip tl

-- |
-- >>> lines ""
-- []
-- >>> lines "abc\n"
-- ["abc"]
-- >>> lines "abc\ndec"
-- ["abc","dec"]
lines :: String -> [String]
lines [] = []
lines s = fst r : (lines $ snd $ break (/= '\n') (snd r))
  where
    r = span (/= '\n') s
    
-- |
-- >>> words ""
-- []
-- >>> words "abc\n"
-- ["abc"]
-- >>> words "abc\ndec"
-- ["abc","dec"]
-- >>> words "abc "
-- ["abc"]
-- >>> words "abc dec"
-- ["abc","dec"]
words :: String -> [String]
words [] = []
words s = fst r : (words $ snd $ break f (snd r))
  where
    f = \x -> x/= '\n' && x/=' '
    r = span f s

-- |
-- >>> unlines []
-- ""
-- >>> unlines ["abc"]
-- "abc\n"
-- >>> unlines ["abc", "bcd"]
-- "abc\nbcd\n"
unlines :: [String] -> String
unlines [] = ""
unlines (a : tl) = a <> "\n" <> unlines tl

-- |
-- >>> unwords []
-- ""
-- >>> unwords ["abc"]
-- "abc"
-- >>> unwords ["abc", "bcd"]
-- "abc bcd"
unwords :: [String] -> String
unwords [] = ""
unwords (a : []) = a
unwords (a : tl) = a <> " " <> unwords tl

-- |
-- >>> nub []
-- []
-- >>> nub [1,2,3]
-- [1,2,3]
-- >>> nub [1,1,2,2,3]
-- [1,2,3]
nub :: Eq a => [a] -> [a]
nub l = foldl inlist [] l
  where
    inlist :: Eq a => [a] -> a -> [a]
    inlist [] a = [a]
    inlist (b : tl) a =
      if a == b
      then inlist tl a
      else b : inlist tl a

-- |
-- >>> delete 1 []
-- []
-- >>> delete 1 [2,3]
-- [2,3]
-- >>> delete 1 [1,1,2,3]
-- [1,2,3]
delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete a (b : tl) =
  if a == b
  then tl
  else b : delete a tl

-- |
-- >>> "Hello World!" \\ "ell W"
-- "Hoorld!"
(\\) :: Eq a => [a] -> [a] -> [a]
(\\) l1 [] = l1
(\\) l1 (a: tl) = (delete a l1) \\ tl

infix 5 \\

-- |
-- >>> union "dog" "cow"
-- "dogcw"
union :: Eq a => [a] -> [a] -> [a]
union l1 l2 = foldr (\x l -> x : delete x l) l2 l1

-- |
-- >>> [1,2,3,4] `intersect` [2,4,6,8]
-- [2,4]
-- >>> [1,2,2,3,4] `intersect` [6,4,4,2]
-- [2,2,4]
intersect :: Eq a => [a] -> [a] -> [a]
intersect l1 l2 = foldr (\x l -> if elem x l2 then x : l else l) [] l1

-- |
-- >>> sort [1,6,4,3,2,5]
-- [1,2,3,4,5,6]
sort :: Ord a => [a] -> [a]
sort [] = []
sort l = sortBy (compare) l

-- |
-- >>> sortBy (compare) [1,6,4,3,2,5]
-- [1,2,3,4,5,6]
sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy _ [] = []
sortBy f (a : tl) = sortBy f first <> (a : sortBy f second)
  where
    (first, second) = partition (\x -> f x a == LT) tl
-- | 追加练习
-- 访问以下 url：
-- http://hackage.haskell.org/package/base-4.14.0.0/docs/Data-List.html
-- 从中选 10 个你认为常用的函数（除去已实现的）实现一下。
-- （两个人各写 5 个不同的加起来就是 10 个）
