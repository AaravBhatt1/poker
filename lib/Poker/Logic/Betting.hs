module Poker.Logic.Betting where

import Control.Lens (ix, preuse, use, (%=), (.=), (<&>), (<~), (^.))
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Loops (whileM_)
import Poker.Core.Action
import Poker.Core.Config
import Poker.Core.Player
import Poker.Core.PlayerID
import Poker.Core.PlayerQueue qualified as PlayerQueue
import Poker.Core.Pot qualified as Pot
import Poker.Monad.PokerMonad
import Poker.State.BettingRoundState
import Poker.State.GameState
import Poker.State.RoundState
import Text.Printf

displayAction :: Action -> PokerM ()
displayAction Action {player = playerID, action = actionType} = do
  -- Get player name and display their action
  playerName <- getPlayer playerID <&> name
  liftIO $ printf "%s %s" playerName (show actionType)

-- | Reset the betting queue for a new round
resetBettingQueue :: PokerM ()
resetBettingQueue =
  do
    -- Cycle until first betting player matches first player in main queue
    firstPlayer <- PlayerQueue.viewFirstPlayerID <$> use playerQueue
    whileM_
      ( (/= firstPlayer)
          . PlayerQueue.viewFirstPlayerID
          <$> use (roundState . bettingState . bettingQueue)
      )
      $ roundState . bettingState . bettingQueue %= PlayerQueue.cycle
    -- Reset number of bets to queue length
    roundState . bettingState . numBetsLeft <~ PlayerQueue.length <$> use (roundState . bettingState . bettingQueue)

-- | Main betting loop that processes player actions
doBetting :: PokerM ()
doBetting =
  do
    whileM_ (use (roundState . bettingState . numBetsLeft) <&> (/= 0)) $
      do
        -- Get next player's action and process it
        playerID <- PlayerQueue.viewFirstPlayerID <$> use (roundState . bettingState . bettingQueue)
        action <- getAction playerID
        displayAction action
        playAction action

        -- Decrement remaining bets and cycle players
        roundState . bettingState . bettingQueue %= PlayerQueue.cycle
        roundState . bettingState . numBetsLeft %= (-) 1

    -- Resets the betting queue order and number of bets left for future betting rounds
    resetBettingQueue

-- | Process the actual betting action (Fold, Check, Call or Raise)
playAction :: Action -> PokerM ()
--
--
playAction (Action {action = Fold}) = do
  -- Remove folded player from the queue
  roundState . bettingState . bettingQueue %= PlayerQueue.removeFirstPlayer
--
--
playAction (Action {action = Check}) = return ()
--
--
playAction (Action {action = Call, player = playerID}) = do
  -- Calculate amount needed to call and process the payment
  pot <- preuse (pots . ix 0) >>= maybe (error "pot lookup error") return
  let currentBet = pot ^. Pot.currentBet
  let moneyInPot = Pot.getPlayerBet playerID pot
  let amountToAdd = currentBet - moneyInPot
  currentPlayer <- getPlayer playerID
  case removeMoney amountToAdd currentPlayer of
    Left err -> throwError err
    Right newPlayer -> players . ix playerID .= newPlayer
  pots . ix 0 %= Pot.addMoney amountToAdd playerID
  playAction (Action {action = Check, player = playerID})
--
--
playAction (Action {action = Raise raiseAmount, player = playerID}) = do
  -- Raise the current bet and then process as a call
  pots . ix 0 %= Pot.raiseCurrentBet raiseAmount
  numActivePlayers <- PlayerQueue.length <$> use (roundState . bettingState . bettingQueue)
  roundState . bettingState . numBetsLeft .= numActivePlayers
  playAction (Action {action = Call, player = playerID})

-- TODO: update this using sockets/handler to get the actual action to be played
getAction :: PlayerID -> PokerM Action
getAction pid = return Action {action = Call, player = pid}

collectBlinds :: PokerM ()
collectBlinds = do
  sb <- use (config . smallBlind)
  bb <- use (config . bigBlind)

  -- Get small blind player
  sbPlayerID <- PlayerQueue.viewFirstPlayerID <$> use playerQueue
  sbPlayer <- getPlayer sbPlayerID

  -- Get big blind player
  bbPlayerID <- PlayerQueue.viewFirstPlayerID . PlayerQueue.cycle <$> use playerQueue
  bbPlayer <- getPlayer bbPlayerID

  -- Collect blinds
  case removeMoney sb sbPlayer of
    Left err -> throwError err
    Right newSBPlayer -> do
      players . ix sbPlayerID .= newSBPlayer
      pots . ix 0 %= Pot.addMoney sb sbPlayerID

  case removeMoney bb bbPlayer of
    Left err -> throwError err
    Right newBBPlayer -> do
      players . ix bbPlayerID .= newBBPlayer
      pots . ix 0 %= Pot.addMoney bb bbPlayerID
