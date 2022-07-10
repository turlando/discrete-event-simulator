{-# LANGUAGE MultiParamTypeClasses #-}

module Simulator.Simulation where

import Simulator.Calendar (Calendar, Entry)
import qualified Simulator.Calendar as Calendar

class Simulation state event where
  transition :: state -> Entry event -> state

foldCalendar'
  :: Simulation state event
  => state
  -> Calendar event
  -> state
foldCalendar' = Calendar.fold' $ flip transition
