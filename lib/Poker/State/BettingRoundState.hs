{-# LANGUAGE TemplateHaskell #-}

module Poker.State.BettingRoundState where

import Control.Lens
import Poker.Core.PlayerQueue (PlayerQueue)

data BettingRoundState = BettingRoundState
  { _bettingQueue :: PlayerQueue,
    _numBetsLeft :: Int
  }

makeLenses ''BettingRoundState
