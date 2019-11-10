
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