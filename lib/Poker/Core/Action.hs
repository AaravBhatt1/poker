module Poker.Core.Action where

import Poker.Core.Money
import Poker.Core.PlayerID

-- | Represents the type of action a player can take in poker
data ActionType
  = -- | Player folds their hand and forfeits the pot
    Fold
  | -- | Player passes when no bet is required
    Check
  | -- | Player matches the current bet
    Call
  | -- | Player increases the bet by the specified amount
    Raise Money
  deriving (Eq)

-- | An action taken by a specific player
data Action = Action
  { -- | The type of action taken
    action :: ActionType,
    -- | The ID of the player taking the action
    player :: PlayerID
  }
  deriving (Eq)
