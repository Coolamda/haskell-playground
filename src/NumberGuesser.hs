module NumberGuesser where

import System.Random
import Control.Monad

-- main = do
--     number <- rollDice
--     putStrLn "Guess a number between 1 and 6:"
--     input <- getLine
--     let guess = read input :: Int
--     when (guess /= number) $ do
--         putStrLn "Wrong number"
--     putStrLn "Ende"

rollDice :: IO Int
rollDice = do
    gen <- getStdGen
    return . fst $ randomR (1,6) gen
    
main = do  
    gen <- getStdGen  
    let (randNumber, _) = randomR (1,10) gen :: (Int, StdGen)     
    putStr "Which number in the range from 1 to 10 am I thinking of? "  
    numberString <- getLine  
    when (not $ null numberString) $ do  
        let number = read numberString  
        if randNumber == number  
            then putStrLn "You are correct!"  
            else putStrLn $ "Sorry, it was " ++ show randNumber  
        newStdGen  
        main  