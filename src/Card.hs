module Card    where

data Suit = Clubs 
            | Diamonds 
            | Hearts 
            | Spades 
            deriving (Bounded, Enum, Eq, Ord, Show)

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
             deriving (Bounded, Enum, Eq, Ord, Show)

data Card = Card Suit Rank deriving (Eq, Ord)

instance Show Card where
  show (Card s r) = show (s) ++ " " ++ show (r)

type Deck = [Card]
type Hand = [Card]
type Group = [Card]

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

nextCard :: Card -> Maybe Card
nextCard (Card s King) = Nothing
nextCard (Card s x) = Just (Card s (succ x))

addToGroup :: Group -> Card -> Maybe Group
addToGroup g c
  | (length g) > 5 = Nothing
  | otherwise = Just (g <> [c])
