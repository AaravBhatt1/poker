module Poker.Logic.Betting where

import Control.Lens (use, (%=), (<&>))
import Control.Monad.Except (throwError)
import Poker.Core.Action
import Poker.Core.Error
import qualified Poker.Core.PlayerQueue as PlayerQueue
import Poker.Monad.PokerMonad
import Poker.State.BettingRoundState
import Poker.State.GameState
import Poker.State.RoundState

playBet :: Action -> PokerM ()
playBet action = do
  betInTurn action
  playAction action

betInTurn :: Action -> PokerM ()
betInTurn (Action {player = playerID}) = do
  currentPlayerID <- use (roundState . bettingState . bettingQueue) <&> PlayerQueue.viewFirstPlayer
  if playerID == currentPlayerID
    then return ()
    else throwError (OutOfTurnBet playerID)

playAction :: Action -> PokerM ()
playAction (Action {action = Fold}) = do
  roundState . bettingState . bettingQueue %= PlayerQueue.removeFirstPlayer
playAction (Action {action = Check}) = do
  roundState . bettingState . bettingQueue %= PlayerQueue.cycle
  roundState . bettingState . numBetsLeft %= (-) 1
playAction _ = pure ()
