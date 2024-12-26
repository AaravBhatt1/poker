module Poker.Logic.Game where

import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Loops (whileM)
import Control.Monad.State (liftIO)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Poker.Core.Config (maxPlayers, minPlayers)
import Poker.Core.Deck qualified as Deck
import Poker.Core.Money
import Poker.Core.Player
import Poker.Core.PlayerQueue qualified as PlayerQueue
import Poker.Core.Pot qualified as Pot
import Poker.Logic.Betting
import Poker.Logic.Deck
import Poker.Logic.PlayerCards
import Poker.Monad.PokerMonad
import Poker.State.BettingRoundState
import Poker.State.GameState
import Poker.State.RoundState

-- Initialize a new game with given players
-- TODO: don't take a list of players and instead have a lobby state
initializeGame :: [Player] -> PokerM ()
initializeGame gamePlayers = do
  let playerMap = Map.fromList [(playerID p, p) | p <- gamePlayers]
  let pQueue = PlayerQueue.fromList $ map playerID gamePlayers
  let initialPot = Pot.Pot (Money 0) Map.empty

  players .= playerMap
  playerQueue .= pQueue
  deck .= Deck.fullDeck
  pots .= [initialPot]
  revealedCards .= Set.empty
  playerCards .= Map.empty

  roundState
    .= RoundState
      { _roundType = PreFlop,
        _bettingState = BettingRoundState pQueue (length gamePlayers)
      }

-- Play a complete round of poker
playRound :: PokerM ()
playRound = do
  -- Reset game state
  resetDeck
  revealedCards .= Set.empty
  dealPlayerCards

  -- Pre-flop betting
  roundState . roundType .= PreFlop
  collectBlinds
  doBetting

  -- Flop
  roundState . roundType .= Flop
  revealCards 3
  doBetting

  -- Turn
  roundState . roundType .= Turn
  revealCards 1
  doBetting

  -- River
  roundState . roundType .= River
  revealCards 1
  doBetting

  -- Showdown
  determineWinner

determineWinner :: PokerM ()
determineWinner = do
  -- Get all remaining players and their hands
  pots' <- use pots
  remainingPlayers <- use players
  cards <- use playerCards
  revealed <- use revealedCards

  -- Calculate best hands and winner
  let remainingPlayerCards = Map.filterWithKey (\pid _ -> pid `Map.member` remainingPlayers) cards
  let bestHands = Map.map (\playerCards' -> getBestHand $ Set.union playerCards' revealed) remainingPlayerCards

  -- TODO: Deal with multiple winning hands
  -- Find winner with highest hand
  let winner =
        fst $
          Map.foldrWithKey
            ( \pid hand (wid, whand) ->
                if hand > whand then (pid, hand) else (wid, whand)
            )
            (head $ Map.keys bestHands, head $ Map.elems bestHands)
            bestHands

  -- Award pot to winner
  let winnings = sum $ map (view Pot.currentBet) pots'
  players . ix winner %= addMoney winnings

-- TODO: broadcast winner

addPlayers :: PokerM ()
addPlayers = do
  -- Add players until the minimum number of players is reached
  -- If there aren't enough players, we will wait
  necessaryPlayers <- whileM canAddNecessaryPlayer readPlayer
  -- Add players until there are either no more left in the queue, or we have reached the maximum amount
  optionalPlayers <- whileM canAddOptionalPlayer readPlayer

  -- This actually adds all these players to the game
  mapM_ addPlayer (necessaryPlayers ++ optionalPlayers)
  where
    canAddOptionalPlayer = do
      maxNumberOfPlayers <- use (config . maxPlayers)
      emptyQueueIO <- atomically . isEmptyTQueue <$> use lobbyQueue
      emptyQueue <- liftIO emptyQueueIO
      hasNotReachedMaxPlayers <- (< maxNumberOfPlayers) . Map.size <$> use players
      return (not emptyQueue || hasNotReachedMaxPlayers)
    canAddNecessaryPlayer = do
      minNumberOfPlayers <- use (config . minPlayers)
      (< minNumberOfPlayers) . Map.size <$> use players
    readPlayer = do
      playerIO <- atomically . readTQueue <$> use lobbyQueue
      liftIO playerIO

addPlayer :: Player -> PokerM ()
addPlayer player = do
  let pid = playerID player
  players %= Map.insert pid player
  playerQueue %= PlayerQueue.add pid
