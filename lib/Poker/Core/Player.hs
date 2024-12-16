module Poker.Core.Player where

import Data.Text (Text)
import Poker.Core.Error
import Poker.Core.Money
import Poker.Core.PlayerID

data Player = Player
  { playerID :: PlayerID,
    name :: Text,
    money :: Money
  }
  deriving (Eq)

instance Show Player where
  show player = show $ name player

addMoney :: Money -> Player -> Player
addMoney moneyToAdd player = player {money = money player + moneyToAdd}

removeMoney :: Money -> Player -> Either PokerError Player
removeMoney moneyToRemove player =
  let currentMoney = money player
   in if moneyToRemove > currentMoney
        then
          Left (InsufficientFunds (playerID player) moneyToRemove)
        else Right (player {money = currentMoney - moneyToRemove})
