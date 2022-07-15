{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Simulator.Time where

import Data.Hashable (Hashable)

newtype Time = Time { unTime :: Float }
  deriving (Eq, Fractional, Hashable, Num, Ord)

instance Show Time where
  show (Time t) = "Time " ++ (show t)
