module Lib
    ( initGame
      , deal
      , shuffle
    ) where

import Card (Hand, Deck, initialDeck)
import Utils (getRandomNumber, replace)
import Permutation (getPermutationAtIndex, numberOfPermutations)
-- import Player

-- players :: [Player]
-- players = [Player {name="A",hand=[]}, Player {name="B",hand=[]}]

deal :: Foldable t => Int -> Int -> t a -> [[a]]
deal nPlayers nCards deck = snd $ foldl (\(count,players) card -> if count < (nCards * nPlayers) then (count+1,(addToPlayer card players count)) else (count, replace players (nPlayers,(players !! (nPlayers)) ++ [card]))) (0,map (\e -> []) [0..(nPlayers)]) deck

addToPlayer :: Foldable t => a -> t [a] -> Int -> [[a]]
addToPlayer card players count = snd $ foldl (\(c,yy) player -> if count `mod` ((length players) - 1) == c then (c+1,yy ++ ([player ++ [card]])) else (c+1,yy ++ [player])) (0,[]) players

isRummy :: Hand -> Bool
isRummy hand = True

shuffle :: Eq a => [a] -> IO [a]
shuffle deck = do
  index <- getRandomNumber 0 (numberOfPermutations deck)
  let newDeck = getPermutationAtIndex deck index
  return newDeck

initGame :: IO ()
initGame = do
    deck <- shuffle initialDeck
    let cx = deal 2 13 deck
    print $ cx !! 0
    print $ length (cx !! 0)
    print $ cx !! 1
    print $ length (cx !! 1)
    print $ cx !! 2
    print $ length (cx !! 2)
