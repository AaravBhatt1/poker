{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Poker.Utils.HandRankingCalc where

-- NOTE: Calc is short for calculator btw

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Poker.Core.Card
import Poker.Core.Hand
import Poker.Core.HandRanking

getHandRanking :: Hand -> HandRanking
getHandRanking hand =
  maximum $
    Maybe.catMaybes
      [ isStraightFlush hand,
        isOfAKind 4 hand,
        isFullHouse hand,
        isFlush hand,
        isStraight hand,
        isOfAKind 3 hand,
        isTwoPair hand,
        isOfAKind 2 hand,
        isHighestCard hand
      ]

type GetRanking = Hand -> Maybe HandRanking

isHighestCard :: GetRanking
isHighestCard (Hand cards) = Just $ HighCard (Set.map rank cards)

isStraightFlush :: GetRanking
isStraightFlush hand =
  if Maybe.isJust $ isFlush hand
    then case isStraight hand of
      Just (Straight highestCard) -> Just (StraightFlush highestCard)
      _ -> Nothing
    else Nothing

isStraight :: GetRanking
isStraight (Hand cards) =
  let ranks = Set.map (fromEnum . rank) cards -- Get sorted ranks from cards
      lowest = Set.findMin ranks
      expectedStraight = Set.fromList [lowest .. lowest + 4]
      highestCard = toEnum $ Set.findMax ranks
   in if ranks == expectedStraight
        then Just $ Straight highestCard
        else Nothing

isFlush :: GetRanking
isFlush (Hand cards) = case Set.size (Set.map suit cards) of
  1 -> Just $ Flush $ Set.map rank cards
  _ -> Nothing

isOfAKind :: Int -> GetRanking
isOfAKind n (Hand cards) =
  let groups = groupRanksByCounts cards
      matching = Map.filter (== n) groups
      singles = Map.filter (== 1) groups
   in case Map.toList matching of
        [(r, _)] -> Just $ case n of
          4 -> FourOfAKind r (fst $ head $ Map.toList singles)
          3 -> ThreeOfAKind r (Set.fromList $ Map.keys singles)
          2 -> Pair r (Set.fromList $ Map.keys singles)
          _ -> error "Invalid number of cards"
        _ -> Nothing

groupRanksByCounts :: Set Card -> Map Rank Int
groupRanksByCounts = Set.foldr (\card acc -> let r = rank card in Map.insertWith (+) r 1 acc) Map.empty

isTwoPair :: GetRanking
isTwoPair (Hand cards) =
  let groups = groupRanksByCounts cards
      pairs = Map.filter (== 2) groups
      singles = Map.filter (== 1) groups
   in if Map.size groups == 3 && Map.size pairs == 2
        then
          let [p1, p2] = Map.toList pairs
              [single] = Map.toList singles
           in Just (TwoPair (fst p1) (fst p2) (fst single))
        else Nothing

isFullHouse :: GetRanking
isFullHouse (Hand cards) =
  let groups = groupRanksByCounts cards
      triplet = Map.filter (== 3) groups
      pair = Map.filter (== 2) groups
   in if Map.size groups == 2
        then
          let [(tRank, _)] = Map.toList triplet
              [(pRank, _)] = Map.toList pair
           in Just (FullHouse tRank pRank)
        else Nothing
