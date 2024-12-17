{-# LANGUAGE TemplateHaskell #-}

-- | Module containing state related to poker rounds
module Poker.State.RoundState where

import Control.Lens
import Poker.State.BettingRoundState

-- | Represents the state of a poker round, including the round type and betting state
data RoundState = RoundState
  { -- | The current round type (e.g. PreFlop, Flop, etc)
    _roundType :: RoundType,
    -- | The current betting state for this round
    _bettingState :: BettingRoundState
  }

-- | The different types of rounds in a poker game
data RoundType
  = -- | Initial round before any community cards
    PreFlop
  | -- | Round after first 3 community cards
    Flop
  | -- | Round after 4th community card
    Turn
  | -- | Final round after 5th community card
    River

makeLenses ''RoundState
