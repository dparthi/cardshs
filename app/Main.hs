module Main where

import Data.List
import System.Random

import Card
import Permutation

-- For a deck of 52 cards:
-- Total Permutations = 80658175170943878571660636856403766975289505440883277824000000000000
-- Number of Members per group at level 1 = 1551118753287382280224243016469303211063259720016986112000000000000

-- this function generates a random number between 0 and the supplied argument
getRandomIndex :: Integer -> IO Integer
getRandomIndex n = do
  g <- getStdGen
  let a = take 1 (randomRs (0 :: Integer, n :: Integer) g)
  return (a !! 0)

main = do
    let deck = initialDeck
    let numberOfPermutations = 80658175170943878571660636856403766975289505440883277824000000000000
    index <- getRandomIndex numberOfPermutations
    let shuffledDeck = getPermutationAtIndex deck index
    print index
    print deck
    print shuffledDeck
