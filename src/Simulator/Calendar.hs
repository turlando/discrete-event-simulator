{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Simulator.Calendar
  ( Calendar (Empty, (:^))
  , empty
  , fromList
  , isEmpty
  , pop
  ) where

import Data.IntPSQ (IntPSQ)
import Simulator.Instant (Instant)
import qualified Data.IntPSQ as PQ
import qualified Simulator.Instant as Instant

newtype Calendar event = Calendar { unCalendar :: IntPSQ () event }
type CalendarEvent event = (Instant, event)

infixr 5 :^

{-# COMPLETE (:^), Empty #-}

pattern Empty :: Calendar event
pattern Empty <- (isEmpty -> True)

pattern (:^) :: CalendarEvent event -> Calendar event -> Calendar event
pattern x :^ xs <- (pop -> Just (x, xs))

empty :: Calendar event
empty = Calendar PQ.empty

fromList :: [CalendarEvent event] -> Calendar event
fromList xs = (Calendar . PQ.fromList)
            $ map (\(instant, event) -> (Instant.toInt instant, (), event)) xs

isEmpty :: Calendar event -> Bool
isEmpty = PQ.null . unCalendar

pop :: Calendar event -> Maybe ((CalendarEvent event), Calendar event)
pop q = (\(key, _prio, value, pq) -> ((Instant.fromInt key, value), Calendar pq))
    <$> (PQ.minView . unCalendar) q
