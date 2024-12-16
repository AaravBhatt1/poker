module Poker.Core.Hand where

import Data.Set (Set)
import Data.Set qualified as Set
import Poker.Core.Card

-- Hands should only be of length 5
newtype Hand = Hand (Set Card)
  deriving (Show, Eq, Ord)

handOfCards :: Set Card -> Hand
handOfCards cards =
  if Set.size cards /= 5
    then error "Hands should only have 5 cards"
    else Hand cards
