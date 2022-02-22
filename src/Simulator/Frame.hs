module Simulator.Frame where

import Simulator.Instant (Instant)

data Frame state event
  = Frame
    { time  :: Instant -- ^ When an event occurred
    , event :: event   -- ^ The occurred event
    , state :: state   -- ^ The resulting state after processing the event
    }
