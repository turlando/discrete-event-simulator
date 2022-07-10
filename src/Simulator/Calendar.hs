module Simulator.Calendar where

import Data.HashPSQ (HashPSQ)
import Simulator.Time (Time)
import qualified Data.HashPSQ as Q

data Entry event = Entry Time event
newtype Calendar event = Calendar (HashPSQ Time () event)

fromList :: [Entry event] -> Calendar event
fromList xs = Calendar $ Q.fromList $ reshape <$> xs
  where
    reshape :: Entry event -> (Time, (), event)
    reshape (Entry time event) = (time, (), event)

fold' :: (Entry event -> a -> a) -> a -> Calendar event -> a
fold' f acc0 (Calendar q) = Q.fold' (\k _p v a -> (f (Entry k v) a)) acc0 q
