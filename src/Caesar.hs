module Caesar where

import Data.List
import Data.Maybe

encode :: String -> String
encode str = map (\c -> moveBy c (length str)) (reverse str)

-- decode :: String -> String

moveBy :: Char -> Int -> Char
moveBy c n
    | n == 0 = c
    | otherwise = moveBy (succ c) (n - 1)

justIndexOfElement :: (Eq a) => a -> [a] -> Int
justIndexOfElement x xs = fromJust $ x `elemIndex` xs
