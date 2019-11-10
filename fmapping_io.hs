
import Data.Char  
import Data.List  
import Prelude hiding (sequenceA)
  
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