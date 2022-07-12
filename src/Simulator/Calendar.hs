module Simulator.Calendar where

import Data.HashPSQ (HashPSQ)
import Simulator.Time (Time)
import qualified Data.HashPSQ as Q

------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

data Entry event = Entry Time event
  deriving (Show)

newtype Calendar event = Calendar (HashPSQ Time () event)
  deriving (Show)


--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

empty :: Calendar event
empty = Calendar $ Q.empty

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


--------------------------------------------------------------------------------
-- Insertion
--------------------------------------------------------------------------------

insert :: Show event => Entry event -> Calendar event -> Calendar event
insert (Entry time event) (Calendar q)
  = case Q.insertView time () event q of
      (Nothing, q')         -> Calendar q'
      ((Just _evicted), _q) ->
        error $ "It's not possible to insert " ++ (show event) ++ " at " ++
                (show time) ++ " because there already is " ++ (show _evicted)


------------------------------------------------------------------------------
-- Query
------------------------------------------------------------------------------

null :: Calendar event -> Bool
null (Calendar q) = Q.null q

head :: Calendar event -> Maybe (Entry event)
head (Calendar q) = reshape <$> Q.findMin q
  where
    reshape :: (Time, (), event) -> Entry event
    reshape (time, (), event) = Entry time event

uncons :: Calendar event -> Maybe ((Entry event), Calendar event)
uncons (Calendar q) = reshape <$> Q.minView q
  where
    reshape
      :: (Time, (), event, HashPSQ Time () event)
      -> (Entry event, Calendar event)
    reshape (time, (), event, rest) = (Entry time event, Calendar rest)


--------------------------------------------------------------------------------
-- Traversals
--------------------------------------------------------------------------------

fold' :: (a -> Entry event -> a) -> a -> Calendar event -> a
fold' f acc0 (Calendar q) = Q.fold' (\k _p v a -> (f a (Entry k v))) acc0 q

scan :: (a -> Entry event -> a) -> a -> Calendar event -> [a]
scan f acc0 c = scanl f acc0 $ toList c
