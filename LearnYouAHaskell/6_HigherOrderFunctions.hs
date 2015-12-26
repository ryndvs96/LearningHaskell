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