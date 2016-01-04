{-- Chapter 8: Making Our Own Types and Typeclasses --}
import qualified Data.Map as Map
{-- 
Algebraic data types intro
data Bool = False | True
--}
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float 
        | Rectangle Point Point
        deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x + a) (y + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = 
    Rectangle (Point (incx x1) (incy y1)) (Point (incx x2) (incy y2))
    where incx = \x -> x + a 
          incy = \y -> y + b
          
baseCircle :: Float -> Shape
baseCircle = Circle (Point 0 0)

baseRectangle :: Float -> Float -> Shape
baseRectangle w h = Rectangle (Point 0 0) (Point w h) 

{-- Record Syntax --}
{--data Person = Person { firstName    :: String
                     , lastName     :: String
                     , age          :: Int
                     , height       :: Float
                     , phoneNumber  :: String
                     , flavor       :: String
                     } deriving (Show)
--}
{-- Type Parameters --}
data Car a b c = Car { company  :: a
                     , model    :: b 
                     , year     :: c 
                     } deriving (Show)
                     
data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i * m) (j * m) (k * m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i * l + j * m + k * n 

{-- deriving other typeclasses --}
data Person = Person { firstName :: String
                     , lastName  :: String
                     , age       :: Int
                     } deriving (Eq, Show, Read)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Eq, Ord, Show, Read, Bounded, Enum)
    
{-- Type Synonyms --}
type PhoneNumber = String
type Name = String    
type PhoneBook = [(Name, PhoneNumber)]

phoneBook :: [(String, String)]
phoneBook = 
    [("betty", "555-2938"),
     ("bonni", "423-1231")]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: LockerMap -> Int -> Either String Code
lockerLookup map lockerNumber =
    case Map.lookup lockerNumber map of
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
        Just (state, code) -> if state /= Taken
                                 then Right code
                                 else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))] 

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 .++
(.++) :: [a] -> [a] -> [a]
[]     .++ ys = ys
(x:xs) .++ ys = x : (xs .++ ys)

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red    == Red    = True
    Green  == Green  = True
    Yellow == Yellow = True
    _      == _      = False
    
instance Show TrafficLight where
    show Red    = "Red light"
    show Yellow = "Yellow light"
    show Green  = "Green light"
    
mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort (x:[]) = [x]
mergeSort list = merge (mergeSort left) (mergeSort right)
    where (left,right) = splitAt (div (length list) 2) list
          merge xs [] = xs
          merge [] xs = xs
          merge left@(x:xs) right@(y:ys) 
            | x < y     = x : (merge xs right)
            | otherwise = y : (merge left ys)
            
            
    