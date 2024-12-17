{-# LANGUAGE TemplateHaskell #-}

module Poker.Core.Pot where

import Control.Lens
import Data.Map (Map)
import Poker.Core.Money
import Poker.Core.PlayerID

data Pot = Pot
  { _currentBet :: Money,
    _playerBets :: Map PlayerID Money -- How much money each person has in the pot
  }
  deriving (Eq)

makeLenses ''Pot

getPlayerBet :: PlayerID -> Pot -> Money
getPlayerBet playerID = view (playerBets . at playerID . non 0)

raiseCurrentBet :: Money -> Pot -> Pot
raiseCurrentBet raiseAmount = over currentBet (+ raiseAmount)

addMoney :: Money -> PlayerID -> Pot -> Pot
addMoney money playerID = over (playerBets . ix playerID) (+ money)
