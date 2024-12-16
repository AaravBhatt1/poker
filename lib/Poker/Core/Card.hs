module Poker.Core.Card where

data Suit = Hearts | Diamonds | Clubs | Spades
  deriving (Eq, Ord) -- NOTE:  Ord is just an arbitrary ordering to optimize set operations

instance Show Suit where
  show Hearts = "♥"
  show Diamonds = "♦"
  show Clubs = "♣"
  show Spades = "♠"

data Rank
  = Two
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
  | Ace
  deriving (Eq, Ord)

instance Show Rank where
  show Two = "2"
  show Three = "3"
  show Four = "4"
  show Five = "5"
  show Six = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine = "9"
  show Ten = "10"
  show Jack = "J"
  show Queen = "Q"
  show King = "K"
  show Ace = "A"

instance Enum Rank where
  fromEnum Ace = 1
  fromEnum Two = 2
  fromEnum Three = 3
  fromEnum Four = 4
  fromEnum Five = 5
  fromEnum Six = 6
  fromEnum Seven = 7
  fromEnum Eight = 8
  fromEnum Nine = 9
  fromEnum Ten = 10
  fromEnum Jack = 11
  fromEnum Queen = 12
  fromEnum King = 13

  toEnum 1 = Ace
  toEnum 2 = Two
  toEnum 3 = Three
  toEnum 4 = Four
  toEnum 5 = Five
  toEnum 6 = Six
  toEnum 7 = Seven
  toEnum 8 = Eight
  toEnum 9 = Nine
  toEnum 10 = Ten
  toEnum 11 = Jack
  toEnum 12 = Queen
  toEnum 13 = King
  toEnum _ = error "Invalid rank value"

data Card = Card
  { rank :: Rank,
    suit :: Suit
  }
  deriving (Eq, Ord)

instance Show Card where
  show (Card rank suit) = show rank ++ show suit
