import Control.Monad.Except
import System.Random  
import Control.Monad.State  
import Control.Monad.Writer
import Data.List (all)
import Data.Ratio  
import Control.Applicative 
import Control.Monad (ap)
randomSt :: (RandomGen g, Random a) => State g a  
randomSt = do
 gen <- get
 let (value,nextGen) = random gen
 put nextGen
 return value
randomSt1 = (runState randomSt (mkStdGen 1)) :: (Int,StdGen)
randomSt2 = (runState randomSt (mkStdGen 2)) :: (Float,StdGen)
threeCoins :: State StdGen (Bool,Bool,Bool)  
threeCoins = do  
 a <- randomSt  
 b <- randomSt  
 c <- randomSt  
 return (a,b,c)  
threeCoins1 = runState threeCoins (mkStdGen 33)
threeCoins2 = runState threeCoins (mkStdGen 2)
liftM :: (Monad m) => (a -> b) -> m a -> m b  
liftM f m = m >>= (\x -> return (f x))  
join :: (Monad m) => m (m a) -> m a  
join mm = do  
    m <- mm  
    m  
keepSmall :: Int -> Writer [String] Bool  
keepSmall x  
    | x < 4 = do  
        tell ["Keeping " ++ show x]  
        return True  
    | otherwise = do  
        tell [show x ++ " is too large, throwing it away"]  
        return False  
powerset :: [a] -> [[a]]  
powerset xs = filterM (\x -> [True, False]) xs  
binSmalls :: Int -> Int -> Maybe Int  
binSmalls acc x  
    | x > 9     = Nothing  
    | otherwise = Just (acc + x)  
foldingFunction :: [Double] -> String -> Maybe [Double]  
foldingFunction (x:y:ys) "*" = return ((x * y):ys)  
foldingFunction (x:y:ys) "+" = return ((x + y):ys)  
foldingFunction (x:y:ys) "-" = return ((y - x):ys)  
foldingFunction xs numberString = Control.Monad.Writer.liftM (:xs) (readMaybe numberString)  
readMaybe :: (Read a) => String -> Maybe a  
readMaybe st = case reads st of [(x,"")] -> Just x  
                                _ -> Nothing  
solveRPN :: String -> Maybe Double  
solveRPN st = do  
    [result] <- foldM foldingFunction [] (words st)  
    return result  
{--in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight
canReachIn3 :: KnightPos -> KnightPos -> Bool  
canReachIn3 start end = end `elem` in3 start 
inMany :: Int -> KnightPos -> [KnightPos]  
inMany x start = return start >>= foldr (<=<) return (replicate x moveKnight)  
canReachIn :: Int -> KnightPos -> KnightPos -> Bool  
canReachIn x start end = end `elem` inMany x start  --}
newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show
instance Functor Prob where  
    fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x,p)) xs  
thisSituation :: Prob (Prob Char)  
thisSituation = Prob  
    [( Prob [('a',1%2),('b',1%2)] , 1%4 )  
    ,( Prob [('c',1%2),('d',1%2)] , 3%4)  
    ]  
flatten :: Prob (Prob a) -> Prob a  
flatten (Prob xs) = Prob $ concat $ map multAll xs  
    where multAll (Prob innerxs,p) = map (\(x,r) -> (x,p*r)) innerxs 

data Coin = Heads | Tails deriving (Show, Eq)  
  
coin :: Prob Coin  
coin = Prob [(Heads,1%2),(Tails,1%2)]  
  
loadedCoin :: Prob Coin  
loadedCoin = Prob [(Heads,1%10),(Tails,9%10)] 
instance Monad Prob where  
    return x = Prob [(x,1%1)]  
    m >>= f = flatten (fmap f m)  
    fail _ = Prob [] 
flipThree :: Prob Bool  
flipThree = do  
    a <- coin  
    b <- coin  
    c <- loadedCoin  
    return (all (==Tails) [a,b,c])  