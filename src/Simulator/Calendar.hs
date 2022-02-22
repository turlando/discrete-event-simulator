{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Simulator.Calendar
  ( Calendar (Empty, (:^))
  , CalendarEvent
  , empty
  , fromList
  , toList
  , isEmpty
  , pop
  ) where

import Data.List (foldl')
import Data.IntPSQ (IntPSQ)
import Simulator.Instant (Instant)
import qualified Data.IntPSQ as PQ
import qualified Simulator.Instant as Instant

newtype Calendar event = Calendar { unCalendar :: IntPSQ () event }

instance (Show event) => Show (Calendar event) where
  show c = show (toList c)

type CalendarEvent event = (Instant, event)

infixr 5 :^

{-# COMPLETE (:^), Empty #-}

pattern Empty :: Calendar event
pattern Empty <- (isEmpty -> True)

pattern (:^) :: CalendarEvent event -> Calendar event -> Calendar event
pattern x :^ xs <- (pop -> Just (x, xs))

empty :: Calendar event
empty = Calendar PQ.empty

-- | Insert new event into the calendar. Return Nothing if there already is an
-- event with the provided Instant.
insert :: CalendarEvent event -> Calendar event -> Maybe (Calendar event)
insert (instant, event) calendar
  = case PQ.insertView (Instant.toInt instant) () event (unCalendar calendar) of
      (Nothing, calendar')         -> Just (Calendar calendar')
      ((Just _evicted), _calendar) -> Nothing

fromList :: [CalendarEvent event] -> Calendar event
fromList xs = foldl' go empty xs
  where
    go :: Calendar event -> CalendarEvent event -> Calendar event
    go calendar calendarEvent = case insert calendarEvent calendar of
      Nothing        -> error "Can't insert two events with same instant"
      Just calendar' -> calendar'

toList :: Calendar event -> [CalendarEvent event]
toList c = (\(key, _prio, value) -> (Instant.fromInt key, value))
       <$> (PQ.toList . unCalendar) c

isEmpty :: Calendar event -> Bool
isEmpty = PQ.null . unCalendar

pop :: Calendar event -> Maybe ((CalendarEvent event), Calendar event)
pop q = (\(key, _prio, value, pq) -> ((Instant.fromInt key, value), Calendar pq))
    <$> (PQ.minView . unCalendar) q
