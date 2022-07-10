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

toList :: Calendar event -> [Entry event]
toList (Calendar q) = reshape <$> Q.toList q
  where
    reshape :: (Time, (), event) -> Entry event
    reshape (time, (), event) = Entry time event

fold' :: (a -> Entry event -> a) -> a -> Calendar event -> a
fold' f acc0 (Calendar q) = Q.fold' (\k _p v a -> (f a (Entry k v))) acc0 q

scan :: (a -> Entry event -> a) -> a -> Calendar event -> [a]
scan f acc0 c = scanl f acc0 $ toList c
