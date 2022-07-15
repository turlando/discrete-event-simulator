{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Data.Foldable (toList)
import Data.Map.Strict (Map)
import Data.Sequence (Seq(Empty, (:<|)), (|>))
import Simulator.Calendar (Calendar, Entry(..))
import Simulator.Simulation (Simulation(..))
import Simulator.Time (Time(..))
import Simulator.Random (exponential)
import System.Random (RandomGen, mkStdGen)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Simulator.Calendar as Calendar
import qualified Text.Pretty.Simple as PP

-- Main ------------------------------------------------------------------------

main :: IO ()
main = pp $ transitionsR (mkStdGen 23) (Time 100)

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
  | NoOperation
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

-- Random parameters -----------------------------------------------------------

arrivalRate :: Float
arrivalRate = 0.3

serviceRate :: Float
serviceRate = 0.5

-- Helpers ---------------------------------------------------------------------

incrementTime :: ClientTimes -> Client -> Time  -> ClientTimes
incrementTime m client amount = Map.insertWith (+) client amount m

incrementTimes :: ClientTimes -> ClientQueue -> Time -> ClientTimes
incrementTimes m clients amount = Map.unionWith (+) clients' m
  where
    clients' = Map.fromList $ (\c -> (c, amount)) <$> toList clients

firstArrival :: Calendar Event -> Maybe (Entry Event)
firstArrival c
  = case Calendar.uncons c of
      Nothing -> Nothing
      Just (e@(Entry _ (Arrival _)), _) -> Just e
      Just ((Entry _ _), c')     -> firstArrival c'

newClient :: Client -> Client
newClient (Client c) = (Client $ c + 1)

entryTime :: Entry event -> Time
entryTime (Entry t _) = t

-- Random helpers --------------------------------------------------------------

randomArrival :: RandomGen g => g -> Time -> Client -> (Entry Event, g)
randomArrival g (Time t) c
  = let (r, g') = exponential g arrivalRate
    in (Entry (Time $ t + r) (Arrival c), g')

randomDeparture :: RandomGen g => g -> Time -> (Entry Event, g)
randomDeparture g (Time t)
  = let (r, g') = exponential g arrivalRate
    in (Entry (Time $ t + r) Departure, g')

randomPair
  :: RandomGen g
  => g
  -> Time
  -> Client
  -> (Entry Event, Entry Event, g)
randomPair g t nc
  = let (a, g')  = randomArrival g t nc
        aTime    = entryTime a
        (d, g'') = randomDeparture g' aTime
    in (a, d, g'')

-- Calendar with NoOperation helpers -------------------------------------------

insert' :: Entry Event -> Time -> Calendar Event -> Calendar Event
insert' entry@(Entry et _) t c =
  if et > t
  then Calendar.insert (Entry t NoOperation) c
  else Calendar.insert entry c

fromList' :: [Entry Event] -> Time -> Calendar Event
fromList' xs t
  = foldr (\x acc -> insert' x t acc) Calendar.empty xs

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
        (NoOperation, _)     -> q

      waitingTimes' = case (event, q) of
        (Arrival c, Empty)     -> Map.insert c 0 wt
        (Arrival _, _ :<| t)   -> incrementTimes wt t dTime
        (Departure, Empty)     -> error "Illegal state"
        (Departure, _ :<| t)   -> incrementTimes wt t dTime
        (NoOperation, Empty)   -> wt
        (NoOperation, _ :<| t) -> incrementTimes wt t dTime

      serviceTimes' = case (event, q) of
        (Arrival c, Empty)     -> Map.insert c 0 st
        (Arrival _, h :<| _)   -> incrementTime st h dTime
        (Departure, Empty)     -> error "Illegal state"
        (Departure, h :<| _)   -> incrementTime st h dTime
        (NoOperation, Empty)   -> wt
        (NoOperation, h :<| _) -> incrementTime st h dTime

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

-- Random ----------------------------------------------------------------------

transitionsR :: RandomGen g => g -> Time -> [State]
transitionsR g endTime
  = transitionsR' g endTime (Time 0) (Client 1) Calendar.empty []

transitionsR'
  :: RandomGen g
  => g
  -> Time
  -> Time
  -> Client
  -> Calendar Event
  -> [State]
  -> [State]
transitionsR' g endTime simTime nextClient calendar []
  = transitionsR' g endTime simTime nextClient calendar [initialState]
transitionsR' g endTime simTime nextClient calendar acc@(s:_)
  | simTime >= endTime = reverse acc
  | otherwise = case Calendar.uncons calendar of
      Nothing ->
        let (a, d, g') = randomPair g simTime nextClient
            c'         = fromList' [a, d] endTime
            client'    = newClient nextClient
        in transitionsR' g' endTime simTime client' c' acc
      Just (entry@(Entry eTime (Arrival _)), calendar') ->
        let s'      = transition s entry
            (a, g') = randomArrival g eTime nextClient
            c'      = insert' a endTime calendar'
            client' = newClient nextClient
        in transitionsR' g' endTime eTime client' c' (s':acc)
      Just (entry@(Entry eTime Departure), calendar') ->
        let s' = transition s entry
            fa = firstArrival calendar'
        in case (Seq.null $ queue s', fa) of
          (True, Nothing) ->
            let (a, d, g') = randomPair g simTime nextClient
                c'         = insert' a endTime $ insert' d endTime calendar'
                client'    = newClient nextClient
            in transitionsR' g' endTime eTime client' c' acc
          (True, Just (Entry naTime _)) ->
            let (d, g') = randomDeparture g naTime
                c'      = insert' d endTime calendar'
            in transitionsR' g' endTime eTime nextClient c' (s':acc)
          (False, _) ->
            let (d, g') = randomDeparture g eTime
                c'      = insert' d endTime calendar'
            in transitionsR' g' endTime eTime nextClient c' (s':acc)
      Just (entry@(Entry eTime NoOperation), calendar') ->
        let s' = transition s entry
        in transitionsR' g endTime eTime nextClient calendar' (s':acc)
