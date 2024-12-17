module Poker.Core.Error where

-- \| Core imports for poker game error types
import Poker.Core.Money
import Poker.Core.PlayerID

-- | All possible errors that can occur during a poker game
data PokerError
  = InsufficientFunds PlayerID Money -- The player that has insufficient funds, and the amount of money they need
  | DeckDrawingError -- Error drawing cards from the deck
  | OutOfTurnBet PlayerID -- A player attempted to bet out of turn
