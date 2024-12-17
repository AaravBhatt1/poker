module Poker.Logic.Deck where

-- \| Module for deck-related game logic operations in poker

import Control.Lens
import Control.Monad.Except (throwError)
import Control.Monad.State
import Data.Set qualified as Set
import Poker.Core.Deck qualified as Deck
import Poker.Monad.PokerMonad
import Poker.State.GameState

-- | Shuffles the current deck in the game state
shuffleDeck :: PokerM ()
shuffleDeck = do
  d <- use deck
  shuffledDeck <- liftIO $ Deck.shuffle d
  deck .= shuffledDeck

-- | Reveals n cards from the deck and adds them to revealed cards set
-- | Returns error if not enough cards remaining
revealCards :: Int -> PokerM ()
revealCards n = do
  d <- use deck
  (cards, newDeck) <- either throwError return $ Deck.drawN n d
  revealedCards %= Set.union cards
  deck .= newDeck
