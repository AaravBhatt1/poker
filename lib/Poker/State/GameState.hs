{-# LANGUAGE TemplateHaskell #-}

-- | Module containing the core game state data type and related functionality
module Poker.State.GameState where

import Control.Lens
import Data.Map (Map)
import Data.Set (Set)
import Poker.Core.Card
import Poker.Core.Deck
import Poker.Core.Player
import Poker.Core.PlayerID
import Poker.Core.Pot (Pot)
import Poker.State.RoundState

-- | Represents the complete state of a poker game
data GameState = GameState
  { -- | Current state of the betting round
    _roundState :: RoundState,
    -- | Mapping of player IDs to player states
    _players :: Map PlayerID Player,
    -- | Community cards that have been revealed
    _revealedCards :: Set Card,
    -- | Current state of the deck
    _deck :: Deck,
    -- | Active pots in the game
    _pots :: [Pot]
  }

-- | Generate lenses for GameState fields
makeLenses ''GameState
