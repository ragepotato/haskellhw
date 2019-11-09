import Data.List 
import Prelude hiding ((.), Maybe, Just, Nothing)
import Data.List hiding (nub)
--import qualified Data.Map 
import qualified Data.List as L
import qualified Data.Map as Map  
import qualified Data.Set as Set
import qualified Data.Char as C
import Data.Function (on)
--import Geometry
--import qualified Geometry.Sphere as Sphere  
--import qualified Geometry.Cuboid as Cuboid  
--import qualified Geometry.Cube as Cube 
doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100
                        then x
                        else x*2
doubleSmallNumber' x = (doubleSmallNumber x) + 1 
conanO'Brien = "It's a-me, Conan O'Brien!"
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x] 
length' xs = sum [1 | _ <- xs]
removeNonUppercase :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']] 
addThree :: Int -> Int -> Int -> Int  
addThree x y z = x + y + z  
factorial :: Integer -> Integer  
factorial n = product [1..n] 
circumference :: Float -> Float  
circumference r = 2 * pi * r  
circumference' :: Double -> Double  
circumference' r = 2 * pi * r  
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN"
lucky x = "Sorry, you're out of luck dawg!"
sayMe :: (Integral a) => a -> String  
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"  
sayMe 4 = "Four!"  
sayMe 5 = "Five!"  
sayMe x = "Not between 1 and 5"
factorials :: (Integral a) => a -> a  
factorials 0 = 1  
factorials n = n * factorials (n - 1) 
charName :: Char -> String  
charName 'a' = "Albert"  
charName 'b' = "Broseph"  
charName 'c' = "Cecil" 
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)  
first :: (a, b, c) -> a  
first (x, _, _) = x  
second :: (a, b, c) -> b  
second (_, y, _) = y  
third :: (a, b, c) -> c  
third (_, _, z) = z  
head' :: [a] -> a  
head' [] = error "Can't call head on an empty list, dummy!"  
head' (x:_) = x  
tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y  
length'' :: (Num b) => [a] -> b  
length'' [] = 0  
length'' (_:xs) = 1 + length' xs  
sum' :: (Num a) => [a] -> a  
sum' [] = 0  
sum' (x:xs) = x + sum' xs  
capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x] 
bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0  
max' :: (Ord a) => a -> a -> a  
max' a b   
        | a > b     = a  
        | otherwise = b 
myCompare :: (Ord a) => a -> a -> Ordering  
a `myCompare` b  
        | a > b     = GT  
        | a == b    = EQ  
        | otherwise = LT 
initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname  
calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0] 
cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea 

head'' :: [a] -> a  
head'' [] = error "No head for empty lists!"  
head'' (x:_) = x 



describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs -> "a longer list." 

maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs
maximum'' :: (Ord a) => [a] -> a  
maximum'' [] = error "maximum of empty list"  
maximum'' [x] = x  
maximum'' (x:xs) = max x (maximum'' xs)  
replicate' :: (Num i, Ord i) => i -> a -> [a]  
replicate' n x  
    | n <= 0    = []  
    | otherwise = x:replicate' (n-1) x  
take' :: (Num i, Ord i) => i -> [a] -> [a]  
take' n _  
    | n <= 0   = []  
take' _ []     = []  
take' n (x:xs) = x : take' (n-1) xs  
reverse' :: [a] -> [a]  
reverse' [] = []  
reverse' (x:xs) = reverse' xs ++ [x] 
repeat' :: a -> [a]  
repeat' x = x:repeat' x  
zip' :: [a] -> [b] -> [(a,b)]  
zip' _ [] = []  
zip' [] _ = []  
zip' (x:xs) (y:ys) = (x,y):zip' xs ys  
elem' :: (Eq a) => a -> [a] -> Bool  
elem' a [] = False  
elem' a (x:xs)  
    | a == x    = True  
    | otherwise = a `elem'` xs  
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted 
multThree :: (Num a) => a -> a -> a -> a  
multThree x y z = x * y * z  
compareWithHundred :: (Num a, Ord a) => a -> Ordering  
compareWithHundred = compare 100 
divideByTen :: (Floating a) => a -> a  
divideByTen = (/10) 
isUpperAlphanum :: Char -> Bool  
isUpperAlphanum = (`elem` ['A'..'Z'])
applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x) 
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
flip' :: (a -> b -> c) -> b -> a -> c  
flip' f = \x y -> f y x   
quicksort' :: (Ord a) => [a] -> [a]    
quicksort' [] = []    
quicksort' (x:xs) =     
    let smallerSorted = quicksort' (filter (<=x) xs)  
        biggerSorted = quicksort' (filter (>x) xs)   
    in  smallerSorted ++ [x] ++ biggerSorted  
largestDivisible :: (Integral a) => a  
largestDivisible = head (filter p [100000,99999..])  
    where p x = x `mod` 3829 == 0  
chain :: (Integral a) => a -> [a]  
chain 1 = [1]  
chain n  
    | even n =  n:chain (n `div` 2)  
    | odd n  =  n:chain (n*3 + 1)  
numLongChains :: Int  
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))
addThree' :: (Num a) => a -> a -> a -> a  
addThree' = \x -> \y -> \z -> x + y + z   
sum''' :: (Num a) => [a] -> a     
sum''' xs = foldl (+) 0 xs  
elem'' :: (Eq a) => a -> [a] -> Bool  
elem'' y ys = foldl (\acc x -> if x == y then True else acc) False ys  
maximums'' :: (Ord a) => [a] -> a  
maximums'' = foldr1 (\x acc -> if x > acc then x else acc)  
reverse'' :: [a] -> [a]  
reverse'' = foldl (\acc x -> x : acc) []  
product'' :: (Num a) => [a] -> a  
product'' = foldr1 (*)  
filter'' :: (a -> Bool) -> [a] -> [a]  
filter'' p = foldr (\x acc -> if p x then x : acc else acc) []  
head''' :: [a] -> a  
head''' = foldr1 (\x _ -> x)  
last'' :: [a] -> a  
last'' = foldl1 (\_ x -> x)
sqrtSums :: Int  
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1
--($) :: (a -> b) -> a -> b  
--f $ x = f x  
--(.) :: (b -> c) -> (a -> b) -> a -> c  
f . g = \x -> f (g x)
fn x = ceiling (negate (tan (cos (max 50 x))))
oddSquareSum :: Integer  
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))  
numUniques :: (Eq a) => [a] -> Int  
numUniques = length . nub 
search :: (Eq a) => [a] -> [a] -> Bool  
search needle haystack =   
    let nlen = length needle  
    in  foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)
encode :: Int -> String -> String  
encode shift msg = 
    let ords = L.map C.ord msg  
        shifted = map (+ shift) ords  
    in  L.map C.chr shifted  
decode :: Int -> String -> String  
decode shift msg = encode (negate shift) msg 
phoneBook =   
    [("betty","555-2938")  
    ,("betty","342-2492")  
    ,("bonnie","452-2928")  
    ,("patsy","493-2928")  
    ,("patsy","943-2929")  
    ,("patsy","827-9162")  
    ,("lucille","205-2928")  
    ,("wendy","939-8282")  
    ,("penny","853-2492")  
    ,("penny","555-2111")  
    ]  
findKey :: (Eq k) => k -> [(k,v)] -> Maybe v  
findKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing  
fromList' :: (Ord k) => [(k,v)] -> Map.Map k v  
fromList' = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty  
phoneBookToMap :: (Ord k) => [(k, a)] -> Map.Map k [a]  
phoneBookToMap xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs  
text1 = "I just had an anime dream. Anime... Reality... Are they so different?"  
text2 = "The old man left his garbage can out and now his trash is all over my lawn!" 
surface :: Shape -> Float  
surface (Circle _ r) = pi * r ^ 2  
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)   
data Point = Point Float Float deriving (Show)  
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)  
nudge :: Shape -> Float -> Float -> Shape  
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r  
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))  
baseCircle :: Float -> Shape  
baseCircle r = Circle (Point 0 0) r  
baseRect :: Float -> Float -> Shape  
baseRect width height = Rectangle (Point 0 0) (Point width height)  
data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     } deriving (Eq, Show, Read)  
data Maybe a = Nothing | Just a 
data Car a b c = Car { company :: a  
                     , model :: b  
                     , year :: c   
                     } deriving (Show)             
tellCar :: (Show a) => Car String String a -> String  
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y  
data Vector a = Vector a a a deriving (Show)  
  
vplus :: (Num t) => Vector t -> Vector t -> Vector t  
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)  
  
vectMult :: (Num t) => Vector t -> t -> Vector t  
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)  
  
scalarMult :: (Num t) => Vector t -> Vector t -> t  
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n  