
import Data.Char  
import Data.List  
import Prelude hiding (sequenceA, Monoid)
import Data.Monoid
import qualified Data.Foldable as F  
main = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine  
          putStrLn line  

data CMaybe a = CNothing | CJust Int a deriving (Show)
instance Functor CMaybe where  
    fmap f CNothing = CNothing  
    fmap f (CJust counter x) = CJust (counter+1) (f x)  

liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c  
liftA2 f a b = f <$> a <*> b  

sequenceA :: (Applicative f) => [f a] -> f [a]  
sequenceA = foldr (liftA2 (:)) (pure [])  
data ZipList a = ZipList { getZipList :: [a] }  
data Profession = Fighter | Archer | Accountant  
data Race = Human | Elf | Orc | Goblin  
data PlayerCharacter = PlayerCharacter Race Profession  
newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)
newtype Pair b a = Pair { getPair :: (a,b) }
instance Functor (Pair c) where  
    fmap f (Pair (x,y)) = Pair (f x, y)  
newtype CoolBool = CoolBool { getCoolBool :: Bool } 
helloMe :: CoolBool -> String  
helloMe (CoolBool _) = "hello"  
type IntList = [Int]  
lengthCompare :: String -> String -> Ordering  
lengthCompare x y = (length x `compare` length y) `mappend`  
                    (vowels x `compare` vowels y) `mappend`  
                    (x `compare` y)  
    where vowels = length . filter (`elem` "aeiou")
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)  
instance F.Foldable Tree where  
    foldMap f Empty = mempty  
    foldMap f (Node x l r) = F.foldMap f l `mappend`  
                             f x           `mappend`  
                             F.foldMap f r  
testTree = Node 5  
            (Node 3  
                (Node 1 Empty Empty)  
                (Node 6 Empty Empty)  
            )  
            (Node 9  
                (Node 8 Empty Empty)  
                (Node 10 Empty Empty)  
            )  
applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b  
applyMaybe Nothing f  = Nothing  
applyMaybe (Just x) f = f x  
type Birds = Int  
type Pole = (Birds,Birds)  
landLeft :: Birds -> Pole -> Maybe Pole  
landLeft n (left,right)  
    | abs ((left + n) - right) < 4 = Just (left + n, right)  
    | otherwise                    = Nothing  

landRight :: Birds -> Pole -> Maybe Pole  
landRight n (left,right)  
    | abs (left - (right + n)) < 4 = Just (left, right + n)  
    | otherwise                    = Nothing  
x -: f = f x  
banana :: Pole -> Maybe Pole  
banana _ = Nothing 
routine :: Maybe Pole  
routine = do  
    start <- return (0,0)  
    first <- landLeft 2 start  
    Nothing
    second <- landRight 2 first  
    landLeft 1 second  
foo :: Maybe String  
foo = do  
    x <- Just 3  
    y <- Just "!"  
    Just (show x ++ y)  
marySue :: Maybe Bool  
marySue = do   
    x <- Just 9  
    Just (x > 8) 
justH :: Maybe Char  
justH = do  
    (x:xs) <- Just "hello"  
    return x  
fail :: (Monad m) => String -> m a  
fail msg = error msg  
wopwop :: Maybe Char  
wopwop = do  
    (x:xs) <- Just ""  
    return x 
listOfTuples :: [(Int,Char)]  
listOfTuples = do  
    n <- [1,2]  
    ch <- ['a','b']  
    return (n,ch)  
class Monad m => MonadPlus m where  
    mzero :: m a  
    mplus :: m a -> m a -> m a  
instance MonadPlus [] where  
    mzero = []  
    mplus = (++)  
guard :: (MonadPlus m) => Bool -> m ()  
guard True = return ()  
guard False = mzero  
sevensOnly :: [Int]  
sevensOnly = do  
    x <- [1..50]  
    guard ('7' `elem` show x)  
    return x  
type KnightPos = (Int,Int) 
moveKnight :: KnightPos -> [KnightPos]  
moveKnight (c,r) = filter onBoard  
    [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
    ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
    ]  
    where onBoard (c,r) = c `elem` [1..8] && r `elem` [1..8] 
in3 :: KnightPos -> [KnightPos]  
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight  
canReachIn3 :: KnightPos -> KnightPos -> Bool  
canReachIn3 start end = end `elem` in3 start  
(<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)  
f <=< g = (\x -> g x >>= f)  