module Poker.Core.Deck where

import Data.Set (Set)
import Data.Set qualified as Set
import Poker.Core.Card
import Poker.Core.Error
import System.Random.Shuffle (shuffleM)

newtype Deck = Deck [Card]

fullDeck :: Deck
fullDeck = Deck [Card r s | r <- allRanks, s <- allSuits]
  where
    allRanks = [Ace .. King]
    allSuits = [Hearts, Diamonds, Spades, Clubs]

-- TODO: replace maybe with either with a proper error
draw :: Deck -> Either PokerError (Card, Deck)
draw (Deck []) = Left DeckDrawingError
draw (Deck (c : cs)) = Right (c, Deck cs)

drawN :: Int -> Deck -> Either PokerError (Set Card, Deck)
drawN n _ | n < 0 = error "Cannot draw a negative number of cards"
drawN 0 deck = Right (Set.empty, deck)
drawN n deck = do
  (c, d) <- draw deck
  (cs, d') <- drawN (n - 1) d
  return (Set.insert c cs, d')

shuffle :: Deck -> IO Deck
shuffle (Deck cards) = do
  shuffledCards <- shuffleM cards
  return (Deck shuffledCards)
