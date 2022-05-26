module Simulator.Simulation where

class Simulation state event where
  transition :: state -> event -> state
