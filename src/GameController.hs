module GameController where

import Table

data GameController = GameController {
  tables :: [Table]
} deriving (Show)
