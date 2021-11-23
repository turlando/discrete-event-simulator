{-# LANGUAGE FunctionalDependencies #-}

module Simulator where

class Simulation state event result
  | state -> result
  , state -> event
  where
  transition :: state -> event -> state
  result :: state -> result

states
  :: Simulation state event result
  => state -> [event] -> [state]
states state events = go [] events
  where
    go (s:ss) (e:es) = go ((transition s e):s:ss) es
    go [] es = go [state] es
    go ss [] = reverse ss
