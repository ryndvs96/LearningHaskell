--------------------------
-- Chapter 5: Recursion --
--------------------------
-- maximum using recursion
maximum1 :: (Ord a) => [a] -> a
maximum1 [] = error "Empty List."
maximum1 (x:[]) = x
maximum1 (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum1 xs

-- Second implementation using Max
maximum2 :: (Ord a) => [a] -> a
maximum2 [] = error "Empty List."
maximum2 (x:[]) = x 
maximum2 (x:xs) = max x (maximum2 xs)

-- Replicate using recursion
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n - 1) x 
 
-- Take using recursion
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' _ [] = []
take' n (x:xs)
    | n <= 0    = []
    | otherwise = x : take' (n - 1) xs 
    
-- reverse using recursion
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- zip to lists together
zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

-- elem in recursion
elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = elem' a xs
    
-- quicksort in haskell
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = lesser ++ [x] ++ greater
    where lesser  = quicksort [ n | n <- xs, n <= x ]
          greater = quicksort [ n | n <- xs, n > x ]