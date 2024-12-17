module Poker.Core.Player where

import Data.Text (Text)
import Poker.Core.Error
import Poker.Core.Money
import Poker.Core.PlayerID

-- | Represents a player in a poker game with an ID, name and current money amount
data Player = Player
  { playerID :: PlayerID,
    name :: Text,
    money :: Money
  }
  deriving (Eq)

-- | Show instance that displays the player's name
instance Show Player where
  show player = show $ name player

-- | Adds money to a player's current balance
addMoney :: Money -> Player -> Player
addMoney moneyToAdd player = player {money = money player + moneyToAdd}

-- | Removes money from a player's balance if they have sufficient funds
-- Returns InsufficientFunds error if the player doesn't have enough money
removeMoney :: Money -> Player -> Either PokerError Player
removeMoney moneyToRemove player =
  let currentMoney = money player
   in if moneyToRemove > currentMoney
        then
          Left (InsufficientFunds (playerID player) moneyToRemove)
        else Right (player {money = currentMoney - moneyToRemove})
