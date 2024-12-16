module Poker.Core.Action where

import Poker.Core.Money
import Poker.Core.PlayerID

data ActionType
  = Fold
  | Check
  | Call
  | Raise Money
  deriving (Eq)

data Action = Action
  { action :: ActionType,
    player :: PlayerID
  }
  deriving (Eq)
