module Card
    ( Hand,
      Deck,
      initialDeck,
      showHand
    ) where

data Suit = Clubs 
            | Diamonds 
            | Hearts 
            | Spades 
            deriving (Show, Eq, Ord)

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

data Card = Card Suit Rank | Joker deriving (Eq, Ord)
instance Show Card where
  show (Card s r) = show (s) ++ " " ++ show (r)

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

showHand :: Hand -> String
showHand (x:[]) = show x ++ "\n"
showHand (x:xs) = show x ++ "\n" ++ showHand(xs)