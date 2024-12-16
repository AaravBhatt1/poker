{-# LANGUAGE TemplateHaskell #-}

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

data GameState = GameState
  { _roundState :: RoundState,
    _players :: Map PlayerID Player,
    _revealedCards :: Set Card,
    _deck :: Deck,
    _pots :: [Pot]
  }

makeLenses ''GameState
