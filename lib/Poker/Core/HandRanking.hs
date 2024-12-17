module Poker.Core.HandRanking where

import Data.Set (Set)
import Poker.Core.Card

-- Data type representing all possible poker hand rankings from highest to lowest
data HandRanking
  = HighCard (Set Rank) -- List of ranks in descending order
  | Pair Rank (Set Rank) -- Pair rank and remaining cards in descending order
  | TwoPair Rank Rank Rank -- Higher pair, lower pair, kicker
  | ThreeOfAKind Rank (Set Rank) -- Three of a kind rank and remaining cards
  | Straight Rank -- Highest card in the straight
  | Flush (Set Rank) -- All ranks in descending order
  | FullHouse Rank Rank -- Three of a kind rank, pair rank
  | FourOfAKind Rank Rank -- Four of a kind rank, remaining card
  | StraightFlush Rank -- Highest card in the straight flush
  deriving (Eq, Ord)

-- Show instance for pretty-printing hand rankings
instance Show HandRanking where
  show (HighCard ranks) = "High Card: " ++ show ranks
  show (Pair p r) = "Pair of " ++ show p ++ " with " ++ show r
  show (TwoPair h l k) = "Two Pair: " ++ show h ++ " and " ++ show l ++ " with kicker " ++ show k
  show (ThreeOfAKind r rs) = "Three of a Kind: " ++ show r ++ " with " ++ show rs
  show (Straight r) = "Straight to " ++ show r
  show (Flush r) = "Flush: " ++ show r
  show (FullHouse t p) = "Full House: " ++ show t ++ " over " ++ show p
  show (FourOfAKind f k) = "Four of a Kind: " ++ show f ++ " with " ++ show k
  show (StraightFlush r) = "Straight Flush to " ++ show r
