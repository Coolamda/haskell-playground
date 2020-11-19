module Heathrow where

main = do input <- readFile "paths.txt"
          print . optimalPath . parse $ input


-- TYPES

data Section = Section { getA :: Int
                       , getB :: Int
                       , getC :: Int
                       } deriving (Show)

type RoadSystem = [Section]

type Path = [(Label, Int)]

data Label = A | B | C deriving (Show)


-- PARSE

sectionsOf3 :: [Int] -> RoadSystem
sectionsOf3 [] = []
sectionsOf3 [a, b, c] = [Section a b c]
sectionsOf3 (a:b:c:xs) = Section a b c : sectionsOf3 xs

parse :: String -> RoadSystem
parse = sectionsOf3 . map readInt . words

readInt :: String -> Int
readInt = read


-- SOLVE

optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
    let (bestPathA, bestPathB) = foldl roadStep ([], []) roadSystem
        cheapestPrizeA = sum $ map snd bestPathA
        cheapestPrizeB = sum $ map snd bestPathB
    in  if cheapestPrizeA <= cheapestPrizeB
           then reverse bestPathA
           else reverse bestPathB

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
    let priceA = sum $ map snd pathA
        priceB = sum $ map snd pathB
        forwardPriceToA = priceA + a
        crossPriceToA = priceB + b + c
        forwardPriceToB = priceB + b
        crossPriceToB = priceB + b + c
        newPathToA = if forwardPriceToA <= crossPriceToA
                        then (A, a) : pathA
                        else (B, b) : (C, c) : pathA
        newPathToB = if forwardPriceToB <= crossPriceToB
                        then (B, b) : pathB
                        else (A, a) : (B, b) : pathB
    in  (newPathToA, newPathToB)
