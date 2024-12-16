module Poker.Core.Pot where

import Data.Map (Map)
import Data.Map qualified as Map
import Poker.Core.Money
import Poker.Core.PlayerID

data Pot = Pot
  { currentBet :: Money,
    playerBets :: Map PlayerID Money -- How much money each person has in the pot
  }
  deriving (Eq)

raiseCurrentBet :: Money -> Pot -> Pot
raiseCurrentBet raiseAmount pot = pot {currentBet = currentBet pot + raiseAmount}

addMoney :: Money -> PlayerID -> Pot -> Pot
addMoney money playerID pot = pot {playerBets = Map.adjust (+ money) playerID (playerBets pot)}
