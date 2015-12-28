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
    
    -- maximum [(x, maximum $ chain x) | x <- [1..1000]]
highChain :: (Integral a) => a -> a -> (a, a)
highChain n m = maximum [(maximum $ chain x, x) | x <- [n..m]]
    
-- Lambdas sole purpose is to be passed on to higher order functions
numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))

-- flip using lambdas expressions
flip1 :: (a -> b -> c) -> b -> a -> c
flip1 f = \x y -> f y x 
-- flip1 with parameter f equals lambda expression
-- with parameters x and y that computes f y x

-- folding, accumulating a list
sum' :: (Num a) => [a] -> a
sum' xs = fold' (\acc x -> acc + x) 0 xs
-- equal to
sum2 :: (Num a) => [a] -> a
sum2 = foldl (+) 0
-- we can omit xs because of currying
-- it will return a function that takes a list
-- generally if you have a function like
------ foo a = bar b a
-- you can rewrite it as
------ foo = bar b 

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

-- right fold!
map2 :: (a -> b) -> [a] -> [b]
map2 f xs = foldr (\x acc -> f x : acc) [] xs

-- attempt to implement fold
fold' :: (a -> a -> a) -> a -> [a] -> a
fold' _ acc [] = acc
fold' f acc (x:xs) = f (fold' f acc xs) x  

-- reverse in folds
reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

(++++) :: (Num a) => a -> a -> a
x ++++ y = x + y + x + y

-- function composition
-- (F o G)(x) = F(G(x))
-- using the '.' function
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- f . g = \x -> f (g x)

fn = ceiling . negate . tan . cos . max 50 