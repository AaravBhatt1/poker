{-# LANGUAGE TemplateHaskell #-}

module Poker.Core.Pot where

import Control.Lens
import Data.Map (Map)
import Poker.Core.Money
import Poker.Core.PlayerID

-- | Represents the pot of money in a poker game
data Pot = Pot
  { -- | The current bet amount that needs to be matched
    _currentBet :: Money,
    -- | How much money each person has in the pot
    _playerBets :: Map PlayerID Money
  }
  deriving (Eq)

makeLenses ''Pot

-- | Gets the amount of money a player has contributed to the pot
-- Returns 0 if the player hasn't bet anything
getPlayerBet :: PlayerID -> Pot -> Money
getPlayerBet playerID = view (playerBets . at playerID . non 0)

-- | Increases the current bet amount in the pot by the given raise amount
raiseCurrentBet :: Money -> Pot -> Pot
raiseCurrentBet raiseAmount = over currentBet (+ raiseAmount)

-- | Adds money to a player's total contribution in the pot
addMoney :: Money -> PlayerID -> Pot -> Pot
addMoney money playerID = over (playerBets . ix playerID) (+ money)
