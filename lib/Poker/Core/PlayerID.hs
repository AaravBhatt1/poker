module Poker.Core.PlayerID where

import Data.Text (Text)

newtype PlayerID = PlayerID Text
  deriving (Eq, Ord, Show) -- NOTE: Order is arbitrary, used for Map operations
