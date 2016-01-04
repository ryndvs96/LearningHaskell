{-- Implementation of a Max Heap using a list --}   
module Heap 
( add
, addList
, contains
, containsList
, peek
, remove
, removeNum
, isHeap
, listToHeap
, swap
, insert
) where
         
add :: (Ord a) => a -> [a] -> [a]
add n [] = [n]
add n heap
    | not $ isHeap heap = error "Must be a heap"
    | otherwise = siftUp (length heap) n (heap ++ [n])
    where siftUp loc val heap
            | loc == 0 || loc == ploc = heap
            | val > parent = 
                siftUp ploc val (swap loc ploc heap)
            | otherwise = heap
            where ploc = (loc - 1) `div` 2
                  parent = heap !! ploc

remove :: (Ord a) => [a] -> [a]
remove [] = error "Must Not Be Null!"
remove heap@(x:xs) 
    | not $ isHeap heap = error "Must be a heap"
    | otherwise = siftDown 0 (last xs) $ (last xs):(init xs)
    where siftDown loc val heap 
              | lloc >= len = heap 
              | rloc >= len = 
                    if (l > val) 
                    then swap lloc loc heap 
                    else heap
              | val < l || val < r =
                    if l > r
                    then siftDown lloc val (swap lloc loc heap)
                    else siftDown rloc val (swap rloc loc heap)
              | otherwise = heap
              where lloc = (loc * 2) + 1
                    rloc = (loc * 2) + 2
                    l = heap !! lloc
                    r = heap !! rloc
          len = length xs
          
addList :: (Ord a) => [a] -> [a] -> [a]
addList [] heap = heap
addList (x:xs) heap = addList xs (add x heap) 

contains :: (Ord a) => a -> [a] -> Bool
contains _ [] = False
contains e (x:xs)
    | e == x    = True
    | otherwise = contains e xs

containsList :: (Ord a) => [a] -> [a] -> Bool
containsList _ [] = False
containsList xs heap = foldl func True xs
    where func = (\acc x -> if contains x heap then acc else False)
    

peek :: (Ord a) => [a] -> a
peek [] = error "Must Not Be Null!"
peek heap@(x:_) 
    | not $ isHeap heap = error "Must be a heap!"
    | otherwise = x

swap :: (Ord a) => Int -> Int -> [a] -> [a]
swap _ _ [] = error "heap must not be null"
swap a b heap 
    | a >= len || b >= len = error "Out of bounds!"
    | otherwise = insert vala b (insert valb a heap)
    where len = length heap
          vala = heap !! a
          valb = heap !! b

insert :: (Ord a) => a -> Int -> [a] -> [a]
insert n loc heap
    | loc >= len = error "Out of bounds!"
    | otherwise = left ++ n:right
    where len = length heap
          (left,(x:right)) = splitAt loc heap

removeNum :: (Ord a) => Int -> [a] -> [a]
removeNum _ [] = []
removeNum 0 heap = heap 
removeNum n heap = removeNum (n - 1) (remove heap)

isHeap :: (Ord a) => [a] -> Bool
isHeap [] = True
isHeap (x:[]) = True
isHeap heap = test 0 heap
    where test loc heap
              | lloc >= len = True
              | rloc >= len = 
                  if l > val then   
                      False
                  else 
                      test lloc heap
              | otherwise   =
                  if r > val || l > val then
                      False
                  else 
                      (test lloc heap) && (test rloc heap)
              where val = heap !! loc
                    lloc = (loc * 2) + 1
                    rloc = (loc * 2) + 2
                    l = heap !! lloc
                    r = heap !! rloc
          len = length heap
          
listToHeap :: (Ord a) => [a] -> [a]
listToHeap [] = []
listToHeap [x] = [x]
listToHeap xs = addList xs []