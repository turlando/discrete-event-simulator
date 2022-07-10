{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Simulator.Time (Time(..))
import Simulator.Calendar (Calendar, Entry(..))
import Simulator.Simulation (Simulation(..))
import qualified Simulator.Calendar as Calendar
import qualified Simulator.Simulation as Simulation

main :: IO ()
main = do
  let result = Simulation.foldCalendar' initialState calendar
  putStrLn $ show result

newtype Client = Client Int
  deriving (Eq, Show)

data Event
  = Arrival Client
  | Departure
  deriving (Show)

data State
  = State
    { time  :: Time
    , count :: Int
    } deriving (Show)

initialState :: State
initialState = State { time = Time 0, count = 0 }

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

instance Simulation State Event where
  transition (State _time clientCount) (Entry eTime event)
    = case event of
        (Arrival _client) ->
          State eTime (clientCount + 1)
        Departure ->
          State eTime (clientCount - 1)
