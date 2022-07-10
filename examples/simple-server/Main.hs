{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Data.Sequence (Seq(Empty, (:<|)), (|>))
import Simulator.Calendar (Calendar, Entry(..))
import Simulator.Simulation (Simulation(..))
import Simulator.Time (Time(..))
import Text.Pretty.Simple (pPrint)
import qualified Data.Sequence as Seq
import qualified Simulator.Calendar as Calendar
import qualified Simulator.Simulation as Simulation

-- Main ------------------------------------------------------------------------

main :: IO ()
main = do
  let result = Simulation.foldCalendar' initialState calendar
  pPrint result

-- Types -----------------------------------------------------------------------

newtype Client = Client Int
  deriving (Eq, Show)

type ClientCount = Int
type ClientQueue = Seq Client

data Event
  = Arrival Client
  | Departure
  deriving (Show)

data State
  = State
    { time  :: Time
    , queue :: ClientQueue
    } deriving (Show)

-- Initial values --------------------------------------------------------------

initialState :: State
initialState
  = State
    { time = Time 0
    , queue = Seq.empty
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
  transition (State _time q) (Entry eTime event)
    = State eTime queue'
    where
      queue' = case (event, q) of
        (Arrival c, Empty)   -> Seq.singleton c
        (Arrival c, _ :<| _) -> q |> c
        (Departure, Empty)   -> error "Illegal state"
        (Departure, _ :<| t) -> t
