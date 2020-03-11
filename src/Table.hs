module Table where

data Table = Table
  { name :: String
  , numPlayers :: Int }
  deriving (Show)

-- addPlayer :: Table -> Player -> Either String Table
-- addPlayer table player
--   | table.numPlayers < 6 = Right (table.numPlayers)
--   | otherwise = Left "Table Full"

-- removePlayer :: Table -> Player -> Table
-- removePlayer table player = delete player table

-- gameState :: TableState
-- gameState 

-- startGame :: [Player] -> IO ()
-- startGame players = initGame
  -- l <- map (\x -> Player {name=(name x), hand=(take 13 sdeck)}) [0..1]
  --                          where sdeck = shuffle deck