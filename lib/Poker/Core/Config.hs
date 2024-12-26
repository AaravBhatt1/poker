{-# LANGUAGE TemplateHaskell #-}

module Poker.Core.Config where

import Control.Lens
import Poker.Core.Money

-- | Configuration for a poker game, containing blind amounts and player limits
data GameConfig = GameConfig
  { -- | Small blind amount
    _smallBlind :: Money,
    -- | Big blind amount
    _bigBlind :: Money,
    -- | Minimum number of players required
    _minPlayers :: Int,
    -- | Maximum number of players allowed
    _maxPlayers :: Int,
    -- | Starting money for each player
    _startingMoney :: Money
  }

makeLenses ''GameConfig

-- | Default game configuration with small blind of 5 and big blind of 10
defaultConfig :: GameConfig
defaultConfig =
  GameConfig
    { _smallBlind = Money 5,
      _bigBlind = Money 10,
      _minPlayers = 2,
      _maxPlayers = 9,
      _startingMoney = Money 1000
    }
