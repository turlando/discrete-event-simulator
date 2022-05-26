module Simulator.Example.SimpleServer where

import Simulator.Simulation (Simulation(..))

data State = State { stateCount :: Int }

data Event
  = Arrival
  | Departure

initialState :: State
initialState = State { stateCount = 0 }

instance Simulation State Event where
  transition (State count) Arrival = State (count + 1)
  transition state Departure = state
