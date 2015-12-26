-----------------------------
-- Chapter 2: Starting Out --
-----------------------------
doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100 then x else doubleMe x
boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
length' xs = sum[1 | _ <- xs]
lowerCase c = if not (c `elem` ['A'..'Z']) then c else ['a'..'z'] !! ((length' ['A'..c]) - 1)
--indexOf obj list = if (not (null list)) && (obj `elem` list) 
--					then sum [1 | c <- list, 
--					else -1
toLower str = [if char `elem` ['A'..'Z'] then (lowerCase char) else char | char <- str]
rightTriangles' x = [(a,b,c) | c <- [1..x], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a + b + c == x]

-- Types and Classes
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
