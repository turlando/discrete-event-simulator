module Simulator.Example.Server where

import Data.Map (Map)
import Data.Sequence (Seq(Empty, (:<|)), (|>))
import Simulator (Simulation)

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Simulator

type Time = Int
type Client = Int
type ClientsCount = Int
type ClientQueue = Seq Client
type ClientsTime = Map Client Time
type Utilization = Map ClientsCount Time

data State
  = State
    { stateCurrentTime  :: Time
    , stateQueue        :: Seq Client
    , stateWaitingTimes :: ClientsTime
    , stateServiceTimes :: ClientsTime
    , stateUtilization  :: Utilization
    }

data Event
  = Event
    { eventTime      :: Time
    , eventEventType :: EventType
    }

data EventType
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
    { stateCurrentTime  = 0
    , stateQueue        = Seq.empty
    , stateWaitingTimes = Map.empty
    , stateServiceTimes = Map.empty
    , stateUtilization  = Map.empty
    }

calendar :: [Event]
calendar
  = [ Event 0 (Arrival 1)
    , Event 2 (Arrival 2)
    , Event 1 Departure
    , Event 2 Departure
    , Event 1 (Arrival 3)
    , Event 1 (Arrival 4)
    , Event 1 (Arrival 5)
    , Event 1 Departure
    , Event 4 Departure
    , Event 2 Departure
    ]

incrementTime :: ClientsTime -> Client -> Time -> ClientsTime
incrementTime m c timeDelta = Map.insertWith (+) c timeDelta m

incrementTimes :: ClientsTime -> Seq Client -> Time -> ClientsTime
incrementTimes m clients timeDelta = go m clients
  where
    go m' (x :<| xs) = go (Map.insertWith (+) x timeDelta m') xs
    go m' Empty = m'

transition :: State -> Event -> State
transition (State currentTime queue waitingTimes serviceTimes utilization)
           (Event time eventType)
  = State currentTime' queue' waitingTimes' serviceTimes' utilization'
  where
    currentTime' = currentTime + time
    queue' = case (eventType, queue) of
      (Arrival c, Empty)   -> Seq.singleton c
      (Arrival c, _ :<| _) -> queue |> c
      (Departure, Empty)   -> error "Illegal state"
      (Departure, _ :<| t) -> t
    waitingTimes' = case (eventType, queue) of
      (Arrival c, Empty)   -> Map.insert c 0 waitingTimes
      (Arrival _, _ :<| t) -> incrementTimes waitingTimes t time
      (Departure, Empty)   -> error "Illegal state"
      (Departure, _ :<| t) -> incrementTimes waitingTimes t time
    serviceTimes' = case (eventType, queue) of
      (Arrival c, Empty)   -> Map.insert c 0 serviceTimes
      (Arrival _, h :<| _) -> incrementTime serviceTimes h time
      (Departure, Empty)   -> error "Illegal state"
      (Departure, h :<| _) -> incrementTime serviceTimes h time
    utilization' = Map.insertWith (+) (Seq.length queue) time utilization

result :: State -> Result
result (State currentTime _queue waitingTimes serviceTimes utilization)
  = Result expectedWaitingTime utilization' expectedQueueLength
  where
    expectedWaitingTime
      = (/) (fromIntegral ((+) (sum (Map.elems waitingTimes))
                               (sum (Map.elems serviceTimes))))
            (fromIntegral (Map.size waitingTimes))
    utilization'
      = (/) (fromIntegral (sum (Map.elems serviceTimes)))
            (fromIntegral currentTime)
    expectedQueueLength
      = (/) (fromIntegral (sum (zipWith (*) (Map.keys utilization)
                                            (Map.elems utilization))))
            (fromIntegral currentTime)

instance Simulator.Simulation State Event Result where
  transition = transition
  result = result

instance Show State where
  show x
    = "{ time:         " <> (show $ stateCurrentTime x)  <> "\n"
   <> ", queue:        " <> (show $ stateQueue x)        <> "\n"
   <> ", waitingTimes: " <> (show $ stateWaitingTimes x) <> "\n"
   <> ", serviceTimes: " <> (show $ stateServiceTimes x) <> "\n"
   <> ", utilization:  " <> (show $ stateUtilization x)  <> "\n"
   <> "}"

instance Show Event where
  show x
    = "{ time: " <> (show $ eventTime x)      <> "\n"
   <> ", type: " <> (show $ eventEventType x) <> "\n"
   <> "}"

instance Show Result where
  show x
    = "{ expectedWaitingTime: " <> (show $ resultExpectedWaitingTime x) <> "\n"
   <> ", utilization:         " <> (show $ resultUtilization x)         <> "\n"
   <> ", expectedQueueLength: " <> (show $ resultExpectedQueueLength x) <> "\n"
   <> "}"
