module Poker.Core.Config (defaultConfig) where

import Poker.Core.Money

data GameConfig = GameConfig
  { smallBlind :: Money,
    bigBlind :: Money
  }

defaultConfig :: GameConfig
defaultConfig =
  GameConfig
    { smallBlind = Money 5,
      bigBlind = Money 10
    }
