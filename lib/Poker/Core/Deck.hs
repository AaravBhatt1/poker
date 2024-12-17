module Poker.Core.Deck where

import Data.Set (Set)
import Data.Set qualified as Set
import Poker.Core.Card
import Poker.Core.Error
import System.Random.Shuffle (shuffleM)

-- | Represents a deck of playing cards
newtype Deck = Deck [Card]

-- | Creates a full deck of 52 playing cards with all combinations of ranks and suits
fullDeck :: Deck
fullDeck = Deck [Card r s | r <- allRanks, s <- allSuits]
  where
    allRanks = [Ace .. King]
    allSuits = [Hearts, Diamonds, Spades, Clubs]

-- | Draws one card from the deck, returning either an error if deck is empty
-- | or the drawn card and remaining deck
draw :: Deck -> Either PokerError (Card, Deck)
draw (Deck []) = Left DeckDrawingError
draw (Deck (c : cs)) = Right (c, Deck cs)

-- | Draws n cards from the deck, returning them as a Set along with remaining deck
-- | Returns error if not enough cards in deck
drawN :: Int -> Deck -> Either PokerError (Set Card, Deck)
drawN n _ | n < 0 = error "Cannot draw a negative number of cards"
drawN 0 deck = Right (Set.empty, deck)
drawN n deck = do
  (c, d) <- draw deck
  (cs, d') <- drawN (n - 1) d
  return (Set.insert c cs, d')

-- | Randomly shuffles the cards in the deck
shuffle :: Deck -> IO Deck
shuffle (Deck cards) = do
  shuffledCards <- shuffleM cards
  return (Deck shuffledCards)
