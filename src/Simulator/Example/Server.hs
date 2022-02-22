module Simulator.Example.Server where

import Data.Function (on)
import Data.Map (Map)
import Data.Sequence (Seq(Empty, (:<|)), (|>))
import Simulator (Simulation)
import Simulator.Calendar (Calendar, CalendarEvent)
import Simulator.Instant (Instant)
import Simulator.Frame (Frame(Frame))

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Simulator
import qualified Simulator.Calendar as Calendar
import qualified Simulator.Instant as Instant

type Client      = Int
type ClientCount = Int
type ClientQueue = Seq Client
type ClientTimes = Map Client Instant
type Utilization = Map ClientCount Instant

data State
  = State
    { stateTime         :: Instant
    , stateQueue        :: Seq Client
    , stateWaitingTimes :: ClientTimes
    , stateServiceTimes :: ClientTimes
    , stateUtilization  :: Utilization
    }

data Event
  = Arrival Client
  | Departure
  deriving (Show)

data Result
  = Result
    { resultExpectedWaitingTime :: Float
    , resultUtilization         :: Float
    , resultExpectedQueueLength :: Float
    }

initialState :: State
initialState
  = State
    { stateTime         = 0
    , stateQueue        = Seq.empty
    , stateWaitingTimes = Map.empty
    , stateServiceTimes = Map.empty
    , stateUtilization  = Map.empty
    }

calendar :: Calendar Event
calendar
  = Calendar.fromList
    [ (0,  Arrival 1)
    , (2,  Arrival 2)
    , (3,  Departure)
    , (5,  Departure)
    , (6,  Arrival 3)
    , (7,  Arrival 4)
    , (8,  Arrival 5)
    , (9,  Departure)
    , (13, Departure)
    , (15, Departure)
    ]

incrementTime :: ClientTimes -> Client -> Instant -> ClientTimes
incrementTime m client timeDelta = Map.insertWith (+) client timeDelta m

incrementTimes :: ClientTimes -> Seq Client -> Instant -> ClientTimes
incrementTimes m clients timeDelta = go m clients
  where
    go m' (x :<| xs) = go (Map.insertWith (+) x timeDelta m') xs
    go m' Empty = m'

transition :: State -> CalendarEvent Event -> State
transition (State previousTime queue waitingTimes serviceTimes utilization)
           (time, event)
  = State time queue' waitingTimes' serviceTimes' utilization'
  where
    timeDelta = time - previousTime

    queue' = case (event, queue) of
      (Arrival c, Empty)   -> Seq.singleton c
      (Arrival c, _ :<| _) -> queue |> c
      (Departure, Empty)   -> error "Illegal state"
      (Departure, _ :<| t) -> t

    waitingTimes' = case (event, queue) of
      (Arrival c, Empty)   -> Map.insert c 0 waitingTimes
      (Arrival _, _ :<| t) -> incrementTimes waitingTimes t timeDelta
      (Departure, Empty)   -> error "Illegal state"
      (Departure, _ :<| t) -> incrementTimes waitingTimes t timeDelta

    serviceTimes' = case (event, queue) of
      (Arrival c, Empty)   -> Map.insert c 0 serviceTimes
      (Arrival _, h :<| _) -> incrementTime serviceTimes h timeDelta
      (Departure, Empty)   -> error "Illegal state"
      (Departure, h :<| _) -> incrementTime serviceTimes h timeDelta

    utilization' = Map.insertWith (+) (Seq.length queue) timeDelta utilization

result :: [Frame State Event] -> Result
result frames
  = Result expectedWaitingTime utilization' expectedQueueLength
  where
    Frame time _event (State _time _queue waitingTimes serviceTimes utilization)
      = last frames

    expectedWaitingTime
      = ((/) `on` fromIntegral)
        (Instant.toInt ((+) (sum (Map.elems waitingTimes))
                            (sum (Map.elems serviceTimes))))
        (Map.size waitingTimes)

    utilization'
      = ((/) `on` (fromIntegral . Instant.toInt))
        (sum (Map.elems serviceTimes))
        time

    expectedQueueLength
      = ((/) `on` fromIntegral)
        (sum (zipWith (*) (Map.keys utilization)
                          (map Instant.toInt (Map.elems utilization))))
        (Instant.toInt time)

instance Simulator.Simulation State Event Result where
  transition = transition
  result = result

instance Show State where
  show (State time queue waitingTimes serviceTimes utilization)
    = "{ time:         " <> show time         <> "\n"
   <> ", queue:        " <> show queue        <> "\n"
   <> ", waitingTimes: " <> show waitingTimes <> "\n"
   <> ", serviceTimes: " <> show serviceTimes <> "\n"
   <> ", utilization:  " <> show utilization  <> "\n"
   <> "}"

instance Show Result where
  show (Result expectedWaitingTime utilization expectedQueueLength)
    = "{ expectedWaitingTime: " <> show expectedWaitingTime <> "\n"
   <> ", utilization:         " <> show utilization         <> "\n"
   <> ", expectedQueueLength: " <> show expectedQueueLength <> "\n"
   <> "}"
