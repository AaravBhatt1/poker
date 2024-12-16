module Poker.Core.PlayerQueue where

import Data.Sequence (Seq (..), (|>))
import Data.Sequence qualified as Seq
import Poker.Core.PlayerID

-- NOTE: This should never be used when empty
newtype PlayerQueue = PlayerQueue (Seq PlayerID)

removeFirstPlayer :: PlayerQueue -> PlayerQueue
removeFirstPlayer (PlayerQueue Empty) = error "Cannot remove from empty queue"
removeFirstPlayer (PlayerQueue (_ :<| rest)) = PlayerQueue rest

viewFirstPlayer :: PlayerQueue -> PlayerID
viewFirstPlayer (PlayerQueue Empty) = error "Cannot view from empty queue"
viewFirstPlayer (PlayerQueue (player :<| _)) = player

cycle :: PlayerQueue -> PlayerQueue
cycle (PlayerQueue Empty) = PlayerQueue Empty
cycle (PlayerQueue (player :<| rest)) = PlayerQueue (rest |> player)

isEmpty :: PlayerQueue -> Bool
isEmpty (PlayerQueue Empty) = True
isEmpty _ = False

length :: PlayerQueue -> Int
length (PlayerQueue queue) = Seq.length queue
