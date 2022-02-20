module Simulator.Instant
  ( Instant
  , fromInt
  , toInt
  ) where

newtype Instant = Instant Int
  deriving (Eq, Show)

fromInt :: Int -> Instant
fromInt i = if i >= 0
            then Instant i
            else error "Instant can't be negative"

toInt :: Instant -> Int
toInt (Instant i) = i

instance Num Instant where
  Instant a + Instant b = fromInt (a + b)
  Instant a - Instant b = fromInt (a - b)
  Instant a * Instant b = fromInt (a * b)
  abs (Instant a) = Instant a
  signum (Instant a) = Instant (signum a)
  fromInteger = fromInt . fromIntegral
