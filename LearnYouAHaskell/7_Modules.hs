{-- Chapter 7: Modules --}

-- import in Haskell File
import Data.List

-- import in global namespace in ghci
--  --  :m + Data.List

-- import several modules in global
--  --  :m + Data.List Data.Map Data.Set

-- import just a couple functions from a module
--  --  import Data.List (nub, sort)

-- import all except a few
--  --  import Data.List hiding (nub)

-- import modules with same name functions
--  --  import qualified Data.Map
-- reference by 
--  --  Data.Map.filter

-- or import like this:
--  --  import qualified Data.Map as M
-- reference by
--  --  M.filter

-- Standard library : 
-- http://www.haskell.org/ghc/docs/latest/html/libraries/
-- locate functions
-- http://haskell.org/hoogle

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

-- using a fold for searching a list for a sublist
search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
    let nlen = length needle
    in  foldl (\acc x -> if take nlen x == needle then True else acc) False $ tails haystack
    
-- finding the largest proper prefix
preSuffix :: (Eq a) => [a] -> [a]
preSuffix [] = []
preSuffix xs = foldl' func [] $ (init . inits) xs
    where func = (\acc x -> if x `isSuffixOf` xs then x else acc)
    
-- failure function
failure :: String -> [Int]
failure [] = error "Must not be null"
failure xs = table
    where table = -1:[length (preSuffix x) | x <- tail (inits xs)]
 