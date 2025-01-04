module Utils where

import Data.Function
import Data.List

adjSublist :: [a] -> Int -> Int -> [a]
adjSublist xs min max = (take (1 + max - min) (drop min xs) )

--member :: Eq a => [a] -> a -> Bool
--member xs elem = any (== elem) xs

remove :: Eq a => [a] -> a -> [a]
remove xs elem = filter (/= elem) xs

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation (x:xs) ys = 
                (length (x:xs) == length ys) 
                && isPermutation (remove xs x) (remove ys x)

uniqueElems :: Eq a => [a] -> Bool
uniqueElems [] = True
uniqueElems (x:xs) = (not (elem x xs)) && uniqueElems xs

sort_by_snd :: Ord b => [(a,b)] -> [(a,b)]
sort_by_snd xs = sortBy (compare `on` snd) xs

inRange :: Int -> Int -> Int -> Bool
inRange x mini maxi = x <= maxi && x >= mini

swap :: Int -> Int -> [a] -> [a]
swap a b xs
    | a == b = xs
    | a < b = (adjSublist xs 0 (a-1)) ++ [xs !! b] ++ (adjSublist xs (a+1) (b-1)) ++ [xs !! a] ++ (adjSublist xs (b+1) (length xs -1))
    | otherwise = (adjSublist xs 0 (b-1)) ++ [xs !! a] ++ (adjSublist xs (b+1) (a-1)) ++ [xs !! b] ++ (adjSublist xs (a+1) (length xs -1))

removeDups :: Eq a => [a] -> [a]
removeDups [] = []
removeDups (x:xs) = x : removeDups (xs2)
    where xs2 = remove xs x

factorial :: Integer -> Integer
factorial n = product [x | x <- [2 .. n]]

