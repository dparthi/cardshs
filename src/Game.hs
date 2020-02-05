module Game where

import Card
import Player

type Game = [Player]

-- startGame :: [Player] -> IO ()
-- startGame players = initGame
  -- l <- map (\x -> Player {name=(name x), hand=(take 13 sdeck)}) [0..1]
  --                          where sdeck = shuffle deck