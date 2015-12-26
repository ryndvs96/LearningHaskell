----------------------------------
-- Chapter 3: Types and Classes --
----------------------------------
removeNonUppercase :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

-- Int is bounded by -2^31 and 2^31 - 1
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- Integer is not bounded
factorial :: Integer -> Integer
factorial n = product [ 1 .. n ]

-- let's try int with factorial
factorialInt :: Int -> Int
factorialInt n = product [ 1 .. n ]

-- Float is a real floating point with single precision
circumference :: Float -> Float
circumference r = 2 * pi * r

-- Double is a floating point with double the precision
circumference' :: Double -> Double
circumference' r = 2 * pi * r

-- Bool is a boolean type (True or False)
-- Char represents a character. Uses single quotes.
-- Tuples have their own types.

-----------------
-- Typeclasses --
-----------------
--ghci> :t (==)  
--(==) :: (Eq a) => a -> a -> Bool  
-- Everything before the => is called a class constraint
-- the two values a and a must be a member of the Eq class

-- Eq 	- provides an interface for testing equality.
-- Ord 	- for types that have ordering. GT, LT, EQ.
-- Show	- represents other types as strings.
-- Read	- opposite of show. Represents strings as types.
-- Enum	- members are types that can be enumerated.
-- Bounded 	- members have an upper and lower bound (Int, char, bool)
-- Num	- numeric typeclasses. (prereq: show, eq)
-- Integral	- only whole numbers (Int, Integer)
-- Floating	- only floating point #s (float, double)
-- -- function 'fromIntegral' helps with conversions