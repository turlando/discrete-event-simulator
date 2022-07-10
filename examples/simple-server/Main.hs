{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Data.Foldable (toList)
import Data.Map.Strict (Map)
import Data.Sequence (Seq(Empty, (:<|)), (|>))
import Simulator.Calendar (Calendar, Entry(..))
import Simulator.Simulation (Simulation(..))
import Simulator.Time (Time(..))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Simulator.Calendar as Calendar
import qualified Simulator.Simulation as Simulation
import qualified Text.Pretty.Simple as PP

-- Main ------------------------------------------------------------------------

main :: IO ()
main = pp (result $ Simulation.foldCalendar' initialState calendar)

pp :: Show a => a -> IO ()
pp = PP.pPrintOpt
     PP.CheckColorTty
     PP.defaultOutputOptionsDarkBg {PP.outputOptionsCompactParens = True}

-- Types -----------------------------------------------------------------------

newtype Client = Client Int
  deriving (Eq, Ord, Show)

type ClientCount      = Int
type ClientQueue      = Seq Client
type ClientTimes      = Map Client Time
type ClientCountTimes = Map ClientCount Time

data Event
  = Arrival Client
  | Departure
  deriving (Show)

data State
  = State
    { time             :: Time
    , queue            :: ClientQueue
    , waitingTimes     :: ClientTimes
    , serviceTimes     :: ClientTimes
    , clientCountTimes :: ClientCountTimes
    } deriving (Show)

data Result
  = Result
    { expectedWaitingTime :: Time
    , utilization         :: Time
    , expectedQueueLength :: Time
    } deriving (Show)

-- Helpers ---------------------------------------------------------------------

incrementTime :: ClientTimes -> Client -> Time  -> ClientTimes
incrementTime m client amount = Map.insertWith (+) client amount m

incrementTimes :: ClientTimes -> ClientQueue -> Time -> ClientTimes
incrementTimes m clients amount = Map.unionWith (+) clients' m
  where
    clients' = Map.fromList $ (\c -> (c, amount)) <$> toList clients

-- Initial values --------------------------------------------------------------

initialState :: State
initialState
  = State
    { time             = Time 0
    , queue            = Seq.empty
    , waitingTimes     = Map.empty
    , serviceTimes     = Map.empty
    , clientCountTimes = Map.empty
    }

calendar :: Calendar Event
calendar
  = Calendar.fromList $ (uncurry Entry) <$>
    [ (0,  Arrival (Client 1))
    , (2,  Arrival (Client 2))
    , (3,  Departure)
    , (5,  Departure)
    , (6,  Arrival (Client 3))
    , (7,  Arrival (Client 4))
    , (8,  Arrival (Client 5))
    , (9,  Departure)
    , (13, Departure)
    , (15, Departure)
    ]

-- Simulation ------------------------------------------------------------------

instance Simulation State Event where
  transition (State sTime q wt st cct) (Entry eTime event)
    = State eTime queue' waitingTimes' serviceTimes' clientCountTimes'
    where
      dTime = eTime - sTime

      queue' = case (event, q) of
        (Arrival c, Empty)   -> Seq.singleton c
        (Arrival c, _ :<| _) -> q |> c
        (Departure, Empty)   -> error "Illegal state"
        (Departure, _ :<| t) -> t

      waitingTimes' = case (event, q) of
        (Arrival c, Empty)   -> Map.insert c 0 wt
        (Arrival _, _ :<| t) -> incrementTimes wt t dTime
        (Departure, Empty)   -> error "Illegal state"
        (Departure, _ :<| t) -> incrementTimes wt t dTime

      serviceTimes' = case (event, q) of
        (Arrival c, Empty)   -> Map.insert c 0 st
        (Arrival _, h :<| _) -> incrementTime st h dTime
        (Departure, Empty)   -> error "Illegal state"
        (Departure, h :<| _) -> incrementTime st h dTime

      clientCountTimes' = Map.insertWith (+) (Seq.length q) dTime cct

result :: State -> Result
result (State t _q wt st cct)
  = Result expectedWaitingTime' utilization' expectedQueueLength'
  where
    expectedWaitingTime'
      = (/) ((sum $ Map.elems wt) + (sum $ Map.elems st))
            (fromIntegral $ Map.size wt)

    utilization' = (sum $ Map.elems st) / t

    expectedQueueLength'
      = (/) (sum $ zipWith (*)
                           (fromIntegral <$> Map.keys cct)
                           (Map.elems cct))
            t
