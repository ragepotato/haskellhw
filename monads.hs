
import Data.Monoid  
import Control.Monad.Writer  
import Control.Monad.Instances
import qualified Control.Monad.State (State)
isBigGang :: Int -> (Bool, String)  
isBigGang x = (x > 9, "Compared gang size to 9.")  
applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)  
applyLog (x,log) f = let (y,newLog) = f x in (y,log `mappend` newLog)  
type Food = String  
type Price = Sum Int  
  
addDrink :: Food -> (Food,Price)  
addDrink "beans" = ("milk", Sum 25)  
addDrink "jerky" = ("whiskey", Sum 99)  
addDrink _ = ("beer", Sum 30)  
newtype Writer w a = Writer { runWriter :: (a, w) }  

gcdReverse :: Int -> Int -> Control.Monad.Writer.Writer [String] Int  
gcdReverse a b  
    | b == 0 = do  
        tell ["Finished with " ++ show a]  
        return a  
    | otherwise = do  
        result <- gcdReverse b (a `mod` b)  
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
        return result  
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }  
toDiffList :: [a] -> DiffList a  
toDiffList xs = DiffList (xs++)  
  
fromDiffList :: DiffList a -> [a]  
fromDiffList (DiffList f) = f []  
finalCountDown :: Int -> Control.Monad.Writer.Writer [String] ()  
finalCountDown 0 = do  
    tell ["0"]  
finalCountDown x = do  
    finalCountDown (x-1)  
    tell [show x]  
addStuff :: Int -> Int  
addStuff x = let  
    a = (*2) x  
    b = (+10) x  
    in a+b  

newtype State s a = State { runState :: s -> (a,s) }  
type Stack = [Int]  
  
pop :: State Stack Int  
pop = Main.State $ \(x:xs) -> (x,xs)  
  
push :: Int -> State Stack ()  
push a = State $ \xs -> ((),a:xs)  
stackStuff :: State Stack ()  
stackStuff = do  
    a <- pop  
    if a == 5  
        then push 5  
        else do  
            push 3  
            push 8  
moreStack :: State Stack ()  
moreStack = do  
    a <- stackManip  
    if a == 100  
        then stackStuff  
        else return ()  

stackyStack :: State Stack ()  
stackyStack = do  
    stackNow <- get  
    if stackNow == [1,2,3]  
        then put [8,3,1]  
        else put [9,2,1]  