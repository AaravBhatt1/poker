module Poker.Logic.Betting where

import Control.Lens (ix, preuse, use, (%=), (.=), (<&>), (^.))
import Control.Monad.Except (throwError)
import Poker.Core.Action
import Poker.Core.Error
import Poker.Core.Player
import Poker.Core.PlayerQueue qualified as PlayerQueue
import Poker.Core.Pot qualified as Pot
import Poker.Monad.PokerMonad
import Poker.State.BettingRoundState
import Poker.State.GameState
import Poker.State.RoundState

playBet :: Action -> PokerM ()
playBet action = do
  betInTurn action
  playAction action

-- TODO: check if the player is actually part of this game and throw a different error if this is the case
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
playAction (Action {action = Call, player = playerID}) = do
  pot <- preuse (pots . ix 0) >>= maybe (error "pot lookup error") return
  let currentBet = pot ^. Pot.currentBet
  let moneyInPot = Pot.getPlayerBet playerID pot
  let amountToAdd = currentBet - moneyInPot
  currentPlayer <- preuse (players . ix playerID) >>= maybe (error "player lookup error") return
  case removeMoney amountToAdd currentPlayer of
    Left err -> throwError err
    Right newPlayer -> players . ix playerID .= newPlayer
  pots . ix 0 %= Pot.addMoney amountToAdd playerID
  playAction (Action {action = Check, player = playerID})
playAction (Action {action = Raise raiseAmount, player = playerID}) = do
  pots . ix 0 %= Pot.raiseCurrentBet raiseAmount
  playAction (Action {action = Call, player = playerID})
