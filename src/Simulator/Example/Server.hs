module Simulator.Example.Server where

import Data.Map (Map)
import Data.Sequence (Seq)
import Simulator (Simulation)

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Simulator

type Time = Int
type Client = Int
type ClientQueue = Seq Client
type ClientsTime = Map Client Time

data EventType = Arrival | Departure

data State
  = State
   { stateCurrentTime  :: Time
   , stateQueue        :: Seq Client
   , stateWaitingTimes :: ClientsTime
   , stateServiceTimes :: ClientsTime
   } deriving (Show)

data Event
  = Event
    { eventTime      :: Time
    , eventEventType :: EventType
    , eventClient    :: Client
    }

data Result
  = Result { resultIsBusy :: Bool }
    deriving (Show)

calendar :: [Event]
calendar
  = [ Event 0 Arrival   1
    , Event 2 Arrival   2
    , Event 2 Departure 1
    , Event 1 Departure 1
    , Event 1 Arrival   3
    , Event 1 Arrival   4
    , Event 1 Arrival   5
    , Event 1 Departure 3
    , Event 4 Departure 4
    , Event 2 Departure 5
    ]

initialState :: State
initialState
  = State { stateCurrentTime  = 0
          , stateQueue        = Seq.empty
          , stateWaitingTimes = Map.empty
          , stateServiceTimes = Map.empty
          }

incrementTime :: ClientsTime -> Client -> Time -> ClientsTime
incrementTime m c timeDelta = Map.insertWith (+) c timeDelta m

incrementTimes :: ClientsTime -> Seq Client -> Time -> ClientsTime
incrementTimes m clients timeDelta = go m clients
  where
    go m' (x Seq.:<| xs) = go (Map.insertWith (+) x timeDelta m') xs
    go m' Seq.Empty = m'

transition :: State -> Event -> State
transition (State currentTime queue waitingTimes serviceTimes)
           (Event time eventType client)
  = State currentTime' queue' waitingTimes' serviceTimes'
  where
    currentTime' = currentTime + time
    queue' = case (eventType, queue) of
      (Arrival, Seq.Empty)     -> Seq.singleton client
      (Arrival, _ Seq.:<| _)   -> queue Seq.|> client
      (Departure, Seq.Empty)   -> error "Illegal state"
      (Departure, _ Seq.:<| t) -> t
    waitingTimes' = case (eventType, queue) of
      (Arrival, Seq.Empty)     -> waitingTimes
      (Arrival, _ Seq.:<| t)   -> incrementTimes waitingTimes t time
      (Departure, Seq.Empty)   -> error "Illegal state"
      (Departure, _ Seq.:<| t) -> incrementTimes waitingTimes t time
    serviceTimes' = case (eventType, queue) of
      (Arrival, Seq.Empty)     -> Map.insert client time serviceTimes
      (Arrival, h Seq.:<| _)   -> incrementTime serviceTimes h time
      (Departure, Seq.Empty)   -> error "Illegal state"
      (Departure, h Seq.:<| _) -> incrementTime serviceTimes h time

instance Simulator.Simulation State Event Result where
  transition = transition
  result = undefined
