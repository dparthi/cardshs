module Utils where

import System.Random
import Data.List

generateRandomNumbers :: Int -> IO [Int]
generateRandomNumbers n = sequence $ map (\x -> getRandomNumber 0 (n-1)) [0..n-1]

getFrequency :: Int -> [Int] -> [Int]
getFrequency _ [] = []
getFrequency n rs = map (\x -> count x rs) [0..n-1]

count :: Int -> [Int] -> Int
count _ [] = 0
count x xs = (length . filter (== x)) xs

getRandomNumber :: Int -> Int -> IO Int
getRandomNumber start end = do
    gen <- newStdGen
    pure $ fst $ randomR (start, end) gen

replace [] _ = []
replace (_:xs) (0,a) = a:xs
replace (x:xs) (n,a) =
    if n < 0
    then (x:xs)
    else x: replace xs (n-1,a)