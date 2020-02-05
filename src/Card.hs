module Card
    ( Hand,
      Deck,
      initialDeck,
      shuffle
    ) where

import System.Random
import Data.List

import Utils

data Suit = Clubs 
            | Diamonds 
            | Hearts 
            | Spades 
            deriving (Show, Eq)

data Rank = Ace
             | Two
             | Three
             | Four
             | Five
             | Six
             | Seven
             | Eight
             | Nine
             | Ten
             | Jack
             | Queen
             | King
             deriving (Show, Eq, Ord)

data Card = Card Suit Rank | Joker deriving (Show)

type Deck = [Card]
type Hand = [Card]

suits :: [Suit]
suits = [Clubs, Diamonds, Hearts, Spades]

ranks :: [Rank]
ranks = [Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King]

makeCard :: Suit -> Rank -> Card
makeCard s r = Card s r

initialDeck :: Deck
initialDeck = concat $ map (\x -> map (makeCard x) ranks) suits

getCardAtIndex :: Deck -> Int -> Card
getCardAtIndex deck index = deck !! index

shuffleArray :: [Int] -> [Integer] -> [Int]
shuffleArray [] _ = []
shuffleArray [x] _ = [x]
shuffleArray list (x:xs) = (fst result) : (shuffleArray (snd result) xs)
                      where result = getAndDeleteFromList x list
shuffleArray _ _ = []

getAndDeleteFromList :: Integer -> [Int] -> (Int, [Int])
getAndDeleteFromList index nums = (a, delete a nums)
                                    where a = nums !! (fromIntegral index)

shuffle :: Deck -> IO [Card]
shuffle deck = do
    g <- newStdGen
    d <- pure $ map (\x -> fst $ randomR (toInteger 0, toInteger x) g) [(length deck - 1), (length deck - 2)..0]
    pure $ map (\x -> getCardAtIndex deck x) (shuffleArray [0..(length deck - 1)] d)