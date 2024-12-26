module Poker.Monad.PokerMonad where

import Control.Lens
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (StateT, runStateT)
import Poker.Core.Error
import Poker.Core.Player
import Poker.Core.PlayerID
import Poker.State.GameState

-- | The main poker monad transformer stack. Combines StateT for game state management
-- with ExceptT for error handling and IO for effects.
type PokerM a = StateT GameState (ExceptT PokerError IO) a

-- | Run a PokerM computation with an initial game state, returning both the result
-- and final state wrapped in Either and IO.
runPokerM :: PokerM a -> GameState -> IO (Either PokerError (a, GameState))
runPokerM action initialState = runExceptT $ runStateT action initialState

-- | Run a PokerM computation but only return the final result value, discarding
-- the final state.
evalPokerM :: PokerM a -> GameState -> IO (Either PokerError a)
evalPokerM action initialState = fmap fst <$> runPokerM action initialState

-- | Run a PokerM computation but only return the final state, discarding the
-- result value.
execPokerM :: PokerM a -> GameState -> IO (Either PokerError GameState)
execPokerM action initialState = fmap snd <$> runPokerM action initialState

getPlayer :: PlayerID -> PokerM Player
getPlayer pid = preuse (players . ix pid) >>= maybe (throwError $ PlayerNotFound pid) return
