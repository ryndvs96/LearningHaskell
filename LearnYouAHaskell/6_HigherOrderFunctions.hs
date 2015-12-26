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

