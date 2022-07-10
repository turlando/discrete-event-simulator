{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Simulator.Time where

import Data.Hashable (Hashable)

newtype Time = Time Float
  deriving (Eq, Hashable, Num, Ord, Show)
