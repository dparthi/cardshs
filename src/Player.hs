module Player where

import Card

data Player = Player { 
  name :: String,
  hand :: Hand 
} deriving (Show)
