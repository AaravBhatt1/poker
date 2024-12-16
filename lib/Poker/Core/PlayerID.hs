module Poker.Core.PlayerID where

import Data.Text (Text)

newtype PlayerID = PlayerID Text
  deriving (Eq, Ord) -- NOTE: Order is arbitrary, used for Map operations
