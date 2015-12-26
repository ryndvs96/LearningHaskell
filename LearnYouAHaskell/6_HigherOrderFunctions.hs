---------------------------------------
-- Chapter 6: Higher Order Functions --
---------------------------------------
-- Curried functions
-- Every function officially takes one parameter.
-- takes in one parameter then applies the second one to it.

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

-- infix partially applied
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

-- functions using functions are parameters
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- zipWith applies some function to two lists zipping them respectively
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- flip takes in a function and flips their parameters
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x 
    
flip2 :: (a -> b -> c) -> b -> a -> c
flip2 f y x = f x y

-- Maps and Filters
-- a map takes a function and list, and applies
-- the function to every element in the list
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map f xs

-- Filters take a predicate (whether something is true or not)
-- and returns a list that satisfies the predicate
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs
    
-- Quicksort using filters
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort(filter (<=x) xs) ++ [x] ++ qsort(filter (>x) xs)

-- Largest # in 1000000 divisible by 3829
largestDivisible :: (Integral a) => a -> a -> a
largestDivisible x y = last (filter p [0..x])
    where p z = z `mod` y == 0
    
-- takeWhile function is like filter but stops after it doesn't hold true anymore

-- Collatz sequences
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n : chain (n `div` 2)
    | odd n  = n : chain (n * 3 + 1)
    
-- Lambdas sole purpose is to be passed on to higher order functions
numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))
