module Poker.Core.PlayerID where

import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID

newtype PlayerID = PlayerID UUID
  deriving (Eq, Ord, Show)

-- Generate a new random UUID for a player
generatePlayerID :: IO PlayerID
generatePlayerID = PlayerID <$> UUID.nextRandom
