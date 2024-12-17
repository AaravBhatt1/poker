{-# LANGUAGE TemplateHaskell #-}

module Poker.State.BettingRoundState where

import Control.Lens
import Poker.Core.PlayerQueue (PlayerQueue)

-- | Represents the state of a betting round in a poker game
data BettingRoundState = BettingRoundState
  { -- | Queue of players still to act in betting round
    _bettingQueue :: PlayerQueue,
    -- | Number of bets remaining in this round
    _numBetsLeft :: Int
  }

-- | Generate lenses for BettingRoundState fields
makeLenses ''BettingRoundState
