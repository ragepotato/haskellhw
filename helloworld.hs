import Data.Char  
import Control.Monad
{-main = do  
    putStrLn "What's your first name?"  
    firstName <- getLine  
    putStrLn "What's your last name?"  
    lastName <- getLine  
    let bigFirstName = map toUpper firstName  
        bigLastName = map toUpper lastName  
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"-}

{-main = do   
    line <- getLine  
    if null line  
        then return ()  
        else do  
            putStrLn $ reverseWords line  
            main  
  
reverseWords :: String -> String  
reverseWords = unwords . map reverse . words  -}

{-main = do  
    return ()  
    return "HAHAHA"  
    line <- getLine  
    return "BLAH BLAH BLAH"  
    return 4  
    putStrLn line  -}

{-main = do   putStr "Hey, "  
            putStr "I'm "  
            putStrLn "Andy!"-}

{-main = do  putChar 't'  
           putChar 'e'  
           putChar 'h'   -}

{-main = do   print True  
            print 2  
            print "haha"  
            print 3.2  
            print [3,4,3]  -}
{-main = do     
    c <- getChar  
    if c /= ' '  
        then do  
            putChar c  
            main  
        else return ()  -}
{-main = do  
    c <- getChar  
    when (c /= ' ') $ do  
        putChar c  
        main  -}
{-main = do  
    rs <- sequence [getLine, getLine, getLine]  
    print rs  -}