module Poker.Core.Error where

import Poker.Core.Money
import Poker.Core.PlayerID

data PokerError
  = InsufficientFunds PlayerID Money -- The player that has insufficient funds, and the amount of money they need
  | DeckDrawingError
  | PlayerLookupError PlayerID
  | OutOfTurnBet PlayerID
