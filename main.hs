import Data.Typeable
import Prelude hiding (catch)
import Control.Exception
import System.Environment
import System.IO

--1.
segments :: [Int] -> [[Int]]
segments  = concatMap inits . tails 
    where inits = takeWhile (not . null) . iterate init
          tails = takeWhile (not . null) . iterate tail
--2.

insert :: (a -> a -> Bool) -> a -> [a] -> [a]
insert _ a [] = [a]
insert f a (a':as)
     | f a a'   = a:a':as
     | otherwise = a':insert f a as

isort2 :: ((Int,Int) -> (Int,Int) -> Bool) -> [(Int,Int)] -> [(Int,Int)]
isort2 _  []      = []
isort2 f (a:as) = insert f a (isort2 f as) 

comparePair:: (Int,Int) -> (Int,Int) -> Bool
comparePair (n1,nn1)(n2,nn2) =  (n1 < n2) || (nn1 == nn2 && n1 <= n2)


--3a.
maxl :: (Ord a) => [a] -> a  
maxl [] = error"empty list"
maxl xs= foldr1 (\x acc -> if x > acc then x else acc) xs


--3b.
member :: (Eq a) => [a] -> a -> Bool
member xs x = foldl (\acc y -> if x == y then True else acc) False xs 
        

--3.
remdup :: [Char]->[Char]
remdup [] = []
remdup (x:xs) = x : remdup (filter (/=x) (x:xs))

elemOcc :: Char->[Char]->Int
elemOcc k = length . filter (==k)

occurrences :: [Char] -> [(Char, Int)]
occurrences xs = zip [x | x <- remdup xs] [elemOcc x xs | x <- remdup xs]

--4.
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x xs -> f x: xs) []

--
{-
pipeline=map.foldr(.)id
built a new function by folding a list of functions together with (.)
using id as the base case.
and then go to map, do map's order


-}

--I am Jason--
--Do you miss me? by Bernie
    
main =do    
    pretest"segments []" $segments []
    pretest"segments [1,2,3]" $segments [1,2,3]
    pretest"maxl [1,3,2,5,6,4]" $maxl [1,3,2,5,6,4]
    pretest"member [1,2,3] 4" $member [1,2,3] 4
    pretest "member [1,2,3,4.0,5] 4" $ member [1,2,3,4.0,5] 4
    pretest "member ['a','b','x','z'] 'b'" $ member ['a','b','x','z'] 'b'
    pretest "occurrences ['a', 'c', 'd', 'a', 'c']" $occurrences ['a', 'c', 'd', 'a', 'c']
    pretest" isort2 comparePair [(5, 4), (3, 2), (1, 5), (2, 6)]" $ isort2 comparePair [(5, 4), (3, 2), (1, 5), (2, 6)]
    where
    pretest p a = putStrLn $ p ++ " = " ++ show a
