------------------------------------
-- Chapter 4: Syntax in Functions --
------------------------------------
-- Pattern matching
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of lucky, pal!"

-- 1 - 5
sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

-- recursive functions with base cases.
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- pattern matching can also fail
charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"

-- pattern matching for tuples
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)

-- other ways to do tuples
addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- first, second, third for 3 tuples
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

-- pattern matching for lists
head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

-- tells us the first elements of the list
tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "This list has one element: " ++ show x
tell (x:y:[]) = "This list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list has more than two elements. The first two elements are: " ++ show x ++ " and " ++ show y

-- length function using recursion
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

-- sum function using recursion
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- AS PATTERNS
capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- Guards!!
bmiTell1 :: (RealFloat a) => a -> String
bmiTell1 bmi
    | bmi <= 18.5   = "You're underweight, you emo, you!"
    | bmi <= 25.0   = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= 30.0   = "You're fat! Lost some weight, fatty!"
    | otherwise     = "You're a whale, congratulations!"

-- Other types of guards
bmiTell2 :: (RealFloat a) => a -> a -> String
bmiTell2 weight height
    | weight / height ^ 2 <= 18.5   = "You're underweight, you emo, you!"
    | weight / height ^ 2 <= 25.0   = "You're supposedly normal. Pffft, I bet you're ugly!"
    | weight / height ^ 2 <= 30.0   = "You're fat! Lost some weight, fatty!"
    | otherwise                     = "You're a whale, congratulations!"    
    
-- Max implemented with guards
max' :: (Ord a) => a -> a -> a
max' a b 
    | a > b     = a
    | otherwise = b 
    
-- myCompare with guards! NOTE the infix
myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b     = GT
    | a == b    = EQ
    | otherwise = LT

-- BMI with a where statement
bmiTell3 :: (RealFloat a) => a -> a -> String
bmiTell3 weight height
    | bmi <= 18.5   = "You're underweight, you emo, you!"
    | bmi <= 25.0   = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= 30.0   = "You're fat! Lost some weight, fatty!"
    | otherwise     = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2 
    
-- BMI with a where statement CONTINUED
bmiTell4 :: (RealFloat a) => a -> a -> String
bmiTell4 weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat    = "You're fat! Lost some weight, fatty!"
    | otherwise     = "You're a whale, congratulations!"
    where   bmi = weight / height ^ 2 
            skinny = 18.5
            normal = 25.0
            fat    = 30.0
            
-- BMI with a where statement CONTINUED AND PATTERN MATCHING
bmiTell5 :: (RealFloat a) => a -> a -> String
bmiTell5 weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat    = "You're fat! Lost some weight, fatty!"
    | otherwise     = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2 
          (skinny, normal, fat) = (18.5, 25.0, 30.0)

-- initials of first and last name
initials :: String -> String -> String
initials first last = [f] ++ "." ++ [l] ++ "."
    where (f:_) = first
          (l:_) = last
          
-- define functions in where blocks
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

-- let bindings in cylinders
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in  sideArea + 2 * topArea
    
-- let bindings inside list comprehension
calcBmis2 :: (RealFloat a) => [(a, a)] -> [a]
calcBmis2 xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

-- cases !
head2 :: [a] -> a
head2 xs = case xs of [] -> error "No head for empty list"
                      (x:_) -> x 

-- cases can be used anywhere almost
describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."
                                               
-- where instead
describeList1 :: [a] -> String
describeList1 xs = "The list is " ++ what xs
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list."