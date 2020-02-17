module Utils where

import System.Random
import Data.List

getRandomNumber :: Integer -> Integer -> IO Integer
getRandomNumber start end = do
    gen <- newStdGen
    pure $ fst $ randomR (start, end) gen

-- 52! = 80658175170943878571660636856403766975289505440883277824000000000000
factorial :: Integer -> Integer
factorial 0 = 1
factorial 1 = 1
factorial n = n * (factorial (n-1))

--------------------------------------------------------------------------------

---------------------- unused functions ----------------------
generateRandomNumbers :: Integer -> IO [Integer]
generateRandomNumbers n = sequence $ map (\x -> getRandomNumber 0 (n-1)) [0..n-1]

getFrequency :: Int -> [Int] -> [Int]
getFrequency _ [] = []
getFrequency n rs = map (\x -> count x rs) [0..n-1]

count :: Int -> [Int] -> Int
count _ [] = 0
count x xs = (length . filter (== x)) xs

replace [] _ = []
replace (_:xs) (0,a) = a:xs
replace (x:xs) (n,a) =
    if n < 0
    then (x:xs)
    else x: replace xs (n-1,a)
--------------------------------------------------------------------------------
