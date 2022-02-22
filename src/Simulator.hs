{-# LANGUAGE FunctionalDependencies #-}

module Simulator
  ( Simulation(transition, result)
  , run
  ) where

import Simulator.Calendar (Calendar(Empty, (:^)), CalendarEvent)
import Simulator.Frame (Frame(Frame))
import qualified Simulator.Calendar as Calendar

class Simulation state event result
  | state -> result
  , state -> event
  where
  transition :: state -> CalendarEvent event -> state
  result :: [Frame state event] -> result

-- | Return the list of all intermediate simulation states.
states
  :: Simulation state event result
  => state -> Calendar event -> [state]
states state Empty = [state]
states state (calendarEvent :^ restOfCalendar)
  = newState : (states newState restOfCalendar)
  where
    newState = transition state calendarEvent

run
  :: Simulation state event result
  => state -> Calendar event -> (result, [Frame state event])
run initialState calendar
  = let states'   = states initialState calendar
        eventList = Calendar.toList calendar
        frames    = zipWith (\(i, e) s -> Frame i e s) eventList states'
    in (result frames, frames)
