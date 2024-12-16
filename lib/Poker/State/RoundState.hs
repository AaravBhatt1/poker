{-# LANGUAGE TemplateHaskell #-}

module Poker.State.RoundState where

import Control.Lens
import Poker.State.BettingRoundState

data RoundState = RoundState
  { _roundType :: RoundType,
    _bettingState :: BettingRoundState
  }

data RoundType
  = PreFlop
  | Flop
  | Turn
  | River

makeLenses ''RoundState
