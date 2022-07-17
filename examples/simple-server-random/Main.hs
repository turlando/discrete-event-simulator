{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Client (Client(..), ClientIdS)
import Control.Monad (replicateM)
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import Data.Sequence (Seq(Empty, (:<|)), (|>))
import Data.Vector (Vector)
import Simulator.Calendar (Calendar, Entry(..))
import Simulator.Simulation (Simulation(..))
import Simulator.Time (Time(..))
import Simulator.Random (exponential)
import Statistics.Distribution (quantile)
import Statistics.Distribution.StudentT (StudentT, studentT)
import Statistics.Sample (meanVarianceUnb)
import System.Random (StdGen, RandomGen, newStdGen)
import qualified Client
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Vector as Vector
import qualified Simulator.Calendar as Calendar
import qualified Text.Pretty.Simple as PP

-- Main ------------------------------------------------------------------------

main :: IO ()
main = analyse 0.95 <$> repeatR resultR 120 (Time 100) >>= pp
-- main = repeatR transitionsR 1 (Time 100) >>= pp

pp :: Show a => a -> IO ()
pp = PP.pPrintOpt
     PP.CheckColorTty
     PP.defaultOutputOptionsDarkBg {PP.outputOptionsCompactParens = True}

-- Types -----------------------------------------------------------------------

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
    , expectedQueueLength :: Float
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
                           (unTime <$> Map.elems cct))
            (unTime t)

-- Analysis --------------------------------------------------------------------

data FinalResult
  = FinalResult
    { finalExpectedWaitingTime :: (Time, Time)
    , finalUtilization         :: (Time, Time)
    , finalExpectedQueueLength :: (Float, Float)
    } deriving (Show)

meanVariance :: [Float] -> (Float, Float)
meanVariance xs = (realToFrac mean, realToFrac variance')
  where
    v :: Vector Double
    v = Vector.fromList (realToFrac <$> xs)

    l :: Int
    l = Vector.length v

    mean, variance :: Double
    (mean, variance) = meanVarianceUnb v

    variance' :: Double
    variance' = sqrt (variance / (fromIntegral l))

analyse :: Float -> [Result] -> FinalResult
analyse confidence xs
  = FinalResult (Time wtm, Time (wtv * t))
                (Time um, Time (uv * t))
                (qlm, qlv * t)
  where
    distribution :: StudentT
    distribution = studentT $ fromIntegral $ length xs

    beta :: Float
    beta = (1 - confidence) / 2

    t :: Float
    t = realToFrac $ quantile distribution $ realToFrac (1 - beta)

    (wtm, wtv) = meanVariance $ (unTime . expectedWaitingTime) <$> xs
    (um, uv)   = meanVariance $ (unTime . utilization) <$> xs
    (qlm, qlv) = meanVariance $ expectedQueueLength <$> xs

-- Random ----------------------------------------------------------------------

repeatR :: (StdGen -> p -> a) -> Int -> p -> IO [a]
repeatR f count endTime = replicateM count act
  where
    act = do
      g <- newStdGen
      return $ f g endTime

resultR :: RandomGen g => g -> Time -> Result
resultR g endTime
  = result'
  where
    (_, result') = transitionsResultR g endTime

transitionsR :: RandomGen g => g -> Time -> [State]
transitionsR g endTime
  = transitions'
  where
    (transitions', _) = transitionsResultR g endTime

transitionsResultR :: RandomGen g => g -> Time -> ([State], Result)
transitionsResultR g endTime = (states, result')
  where
    transitions' = Client.evalS
                   (transitionsR' g endTime (Time 0) Calendar.empty [])
                   (Client 1)
    finalState   = head transitions'
    states       = reverse transitions'
    result'      = result finalState

transitionsR'
  :: RandomGen g
  => g
  -> Time
  -> Time
  -> Calendar Event
  -> [State]
  -> ClientIdS [State]
transitionsR' g endTime simTime calendar []
  = transitionsR' g endTime simTime calendar [initialState]
transitionsR' g endTime simTime calendar acc@(s:_)
  | simTime >= endTime = return acc
  | otherwise = case Calendar.uncons calendar of
      Nothing -> Client.next >>= \nextClient ->
        let (a, d, g') = randomPair g simTime nextClient
            c'         = fromList' [a, d] endTime
        in transitionsR' g' endTime simTime c' acc
      Just (entry@(Entry eTime (Arrival _)), calendar') ->
        Client.next >>= \nextClient ->
          let s'      = transition s entry
              (a, g') = randomArrival g eTime nextClient
              c'      = insert' a endTime calendar'
          in transitionsR' g' endTime eTime c' (s':acc)
      Just (entry@(Entry eTime Departure), calendar') ->
        let s' = transition s entry
            fa = firstArrival calendar'
        in case (Seq.null $ queue s', fa) of
          (True, Nothing) -> Client.next >>= \nextClient ->
            let (a, d, g') = randomPair g simTime nextClient
                c'         = insert' a endTime $ insert' d endTime calendar'
            in transitionsR' g' endTime eTime c' acc
          (True, Just (Entry naTime _)) -> 
            let (d, g') = randomDeparture g naTime
                c'      = insert' d endTime calendar'
            in transitionsR' g' endTime eTime c' (s':acc)
          (False, _) -> 
            let (d, g') = randomDeparture g eTime
                c'      = insert' d endTime calendar'
            in transitionsR' g' endTime eTime c' (s':acc)
      Just (entry@(Entry eTime NoOperation), calendar') ->
        let s' = transition s entry
        in transitionsR' g endTime eTime calendar' (s':acc)
