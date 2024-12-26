module Poker.Logic.PlayerCards where

import Control.Lens
import Control.Monad.Except (throwError)
import Data.Foldable
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Poker.Core.Card
import Poker.Core.Deck qualified as Deck
import Poker.Core.Hand
import Poker.Core.HandRanking
import Poker.Core.PlayerQueue qualified as PlayerQueue
import Poker.Monad.PokerMonad
import Poker.State.GameState
import Poker.Utils.HandRankingCalc

-- Deal two cards to each player
dealPlayerCards :: PokerM ()
dealPlayerCards = do
  pq <- use playerQueue
  let players' = toList $ PlayerQueue.toSeq pq
  mapM_ dealToPlayer players'
  where
    dealToPlayer pid = do
      d <- use deck
      case Deck.drawN 2 d of
        Left err -> throwError err
        Right (cards, newDeck) -> do
          playerCards %= Map.insert pid cards
          deck .= newDeck

-- Get the best possible 5-card hand from a player's hole cards and community cards
getBestHand :: Set Card -> HandRanking
getBestHand cards = maximum $ map getHandRanking allPossibleHands
  where
    allPossibleHands =
      [ handOfCards h | h <- Set.toList $ Set.powerSet cards, Set.size h == 5
      ]
