module Poker.Utils.MonadUtils where

import Control.Lens
import Data.Map qualified as Map
import Poker.Core.Player
import Poker.Monad.PokerMonad
import Poker.State.GameState

putPlayer :: Player -> PokerM ()
putPlayer player = do
  let pID = playerID player
  players %= Map.insert pID player
