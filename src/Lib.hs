module Lib
    ( initGame
      , deal
      , shuffle
    ) where

import Data.List (sort, delete)
import Card (Hand, Deck, initialDeck, showHand)
import Utils (getRandomNumber, replace)
import Permutation (getPermutationAtIndex, numberOfPermutations)

import Player

-- players :: [Player]
-- players = [Player {name="A",hand=[]}, Player {name="B",hand=[]}]

deal :: Foldable t => Int -> Int -> t a -> [[a]]
deal nPlayers nCards deck =
  snd $
  foldl (
    \(count,players) card ->
      if count < (nCards * nPlayers)
        then (count+1,(addToPlayer card players count))
        else (count+1, replace players (nPlayers,(players !! nPlayers) ++ [card]))
  ) (0,map (\e -> []) [0..nPlayers]) deck

addToPlayer :: Foldable t => a -> t [a] -> Int -> [[a]]
addToPlayer card players count =
  snd $
  foldl (
    \(c,yy) player ->
      if count `mod` ((length players) - 1) == c
        then (c+1,yy ++ ([player ++ [card]]))
        else (c+1,yy ++ [player])
  ) (0,[]) players

isRummy :: Hand -> Bool
isRummy [] = False
isRummy hand = foldl (\acc card -> True) False (sort hand)
-- 3-3-3-4
-- 3-5-5
-- 4-4-5


shuffle :: Eq a => [a] -> IO [a]
shuffle deck = do
  index <- getRandomNumber 0 (numberOfPermutations deck)
  let newDeck = getPermutationAtIndex deck index
  return newDeck

initGame :: IO ()
initGame = do
  let d = initialDeck
  deck <- shuffle $ d
  let cx = deal 2 13 deck

  let p1 = Player {name="Computer", hand=cx !! 0}
  let p2 = Player {name="Human", hand=cx !! 1}
  let pile = cx !! 2

  putStrLn "Player 1"
  putStr $ showHand $ sort $ hand p1
  putStrLn "-----------------------"

  putStrLn "Player 2"
  putStr $ showHand $ sort $ hand p2
  putStrLn "-----------------------"

  putStrLn "Pile"
  putStr $ showHand $ pile
  putStrLn "-----------------------"

  let openCard = pile !! 0
  putStrLn $ "Open Card: " <> show openCard
  putStrLn "-----------------------"

  let pile1 = delete openCard pile
  let openCard = pile1 !! 0
  putStrLn $ "Open Card: " <> show openCard
  putStrLn "-----------------------"
