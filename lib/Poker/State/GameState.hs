{-# LANGUAGE TemplateHaskell #-}

-- | Module containing the core game state data type and related functionality
module Poker.State.GameState where

import Control.Concurrent.STM
import Control.Lens
import Data.Map (Map)
import Data.Set (Set)
import Poker.Core.Card
import Poker.Core.Config (GameConfig)
import Poker.Core.Deck
import Poker.Core.Player
import Poker.Core.PlayerID
import Poker.Core.PlayerQueue
import Poker.Core.Pot (Pot)
import Poker.State.RoundState

-- | Represents the complete state of a poker game
data GameState = GameState
  { -- | Current state of the betting round
    _roundState :: RoundState,
    -- | Mapping of player IDs to player states
    _players :: Map PlayerID Player,
    -- | The order in which the players play in a round
    _playerQueue :: PlayerQueue,
    -- | Community cards that have been revealed
    _revealedCards :: Set Card,
    -- | Current state of the deck
    _deck :: Deck,
    -- | Active pots in the game
    _pots :: [Pot],
    -- | Mapping of player IDs to their cards
    _playerCards :: Map PlayerID (Set Card),
    -- | The config of the game
    _config :: GameConfig,
    -- | The channel of the people that are trying to join the game
    _lobbyQueue :: TQueue Player
  }

-- | Generate lenses for GameState fields
makeLenses ''GameState
