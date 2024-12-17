module Poker.Core.Hand where

-- \| Import the Set data type and qualified operations
import Data.Set (Set)
import Data.Set qualified as Set
-- \| Import card-related types and functions
import Poker.Core.Card

-- | The Hand type represents a poker hand which must contain exactly 5 cards
newtype Hand = Hand (Set Card)
  deriving (Show, Eq, Ord)

-- | Creates a Hand from a Set of Cards
-- | Throws an error if the number of cards is not exactly 5
handOfCards :: Set Card -> Hand
handOfCards cards =
  if Set.size cards /= 5
    then error "Hands should only have 5 cards"
    else Hand cards
