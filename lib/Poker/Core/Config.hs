module Poker.Core.Config (defaultConfig) where

import Poker.Core.Money

-- | Configuration for a poker game, containing blind amounts
data GameConfig = GameConfig
  { -- | Small blind amount
    smallBlind :: Money,
    -- | Big blind amount
    bigBlind :: Money
  }

-- | Default game configuration with small blind of 5 and big blind of 10
defaultConfig :: GameConfig
defaultConfig =
  GameConfig
    { smallBlind = Money 5,
      bigBlind = Money 10
    }
