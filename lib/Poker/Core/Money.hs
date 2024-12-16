module Poker.Core.Money where

import Numeric.Natural

newtype Money = Money Natural
    deriving (Eq, Ord, Num)

instance Show Money where
    show (Money money) = "£" ++ show money