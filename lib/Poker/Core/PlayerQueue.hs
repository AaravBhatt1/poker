module Poker.Core.PlayerQueue where

import Data.Sequence (Seq (..), (|>))
import Data.Sequence qualified as Seq
import Poker.Core.PlayerID

-- NOTE: This should never be used when empty
newtype PlayerQueue = PlayerQueue {toSeq :: Seq PlayerID}

-- Remove and discard the first player from the queue
removeFirstPlayer :: PlayerQueue -> PlayerQueue
removeFirstPlayer (PlayerQueue Empty) = error "Cannot remove from empty queue"
removeFirstPlayer (PlayerQueue (_ :<| rest)) = PlayerQueue rest

-- Get the ID of the first player in the queue without removing them
viewFirstPlayerID :: PlayerQueue -> PlayerID
viewFirstPlayerID (PlayerQueue Empty) = error "Cannot view from empty queue"
viewFirstPlayerID (PlayerQueue (player :<| _)) = player

-- Move the first player to the back of the queue
cycle :: PlayerQueue -> PlayerQueue
cycle (PlayerQueue Empty) = PlayerQueue Empty
cycle (PlayerQueue (player :<| rest)) = PlayerQueue (rest |> player)

-- Check if the queue is empty
isEmpty :: PlayerQueue -> Bool
isEmpty (PlayerQueue Empty) = True
isEmpty _ = False

-- Get the number of players in the queue
length :: PlayerQueue -> Int
length (PlayerQueue queue) = Seq.length queue

-- Create a new PlayerQueue from a list of PlayerIDs
fromList :: [PlayerID] -> PlayerQueue
fromList = PlayerQueue . Seq.fromList

add :: PlayerID -> PlayerQueue -> PlayerQueue
add pid (PlayerQueue pq) = PlayerQueue (pid :<| pq)
