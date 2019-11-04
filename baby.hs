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
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"  
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise                 = "You're a whale, congratulations!"
max' :: (Ord a) => a -> a -> a  
max' a b   
        | a > b     = a  
        | otherwise = b 
myCompare :: (Ord a) => a -> a -> Ordering  
a `myCompare` b  
        | a > b     = GT  
        | a == b    = EQ  
        | otherwise = LT 