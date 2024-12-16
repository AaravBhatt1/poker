module Poker.Logic.Deck where

import Control.Lens
import Control.Monad.Except (throwError)
import Control.Monad.State
import Data.Set qualified as Set
import Poker.Core.Deck qualified as Deck
import Poker.Monad.PokerMonad
import Poker.State.GameState

shuffleDeck :: PokerM ()
shuffleDeck = do
  d <- use deck
  shuffledDeck <- liftIO $ Deck.shuffle d
  deck .= shuffledDeck

revealCards :: Int -> PokerM ()
revealCards n = do
  d <- use deck
  (cards, newDeck) <- either throwError return $ Deck.drawN n d
  revealedCards %= Set.union cards
  deck .= newDeck
