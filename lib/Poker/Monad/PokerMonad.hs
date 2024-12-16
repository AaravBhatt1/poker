module Poker.Monad.PokerMonad where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (StateT, runStateT)
import Poker.Core.Error
import Poker.State.GameState

type PokerM a = StateT GameState (ExceptT PokerError IO) a

runPokerM :: PokerM a -> GameState -> IO (Either PokerError (a, GameState))
runPokerM action initialState = runExceptT $ runStateT action initialState

evalPokerM :: PokerM a -> GameState -> IO (Either PokerError a)
evalPokerM action initialState = fmap fst <$> runPokerM action initialState

execPokerM :: PokerM a -> GameState -> IO (Either PokerError GameState)
execPokerM action initialState = fmap snd <$> runPokerM action initialState
