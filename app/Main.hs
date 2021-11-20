module Main where

import Data.List (intercalate)
import Control.Monad (forM_)

import qualified Simulator
import qualified Simulator.Example.Server as Server

indent :: Int -> String -> String
indent spaces string
  = intercalate "\n" $ map (\l -> replicate spaces ' ' <> l) (lines string)

main :: IO ()
main = do
  let states  = Simulator.states Server.initialState Server.calendar
      states' = tail states
      result  = Simulator.result $ last states

  putStrLn "Initial state:"
  putStrLn $ indent 4 $ show $ Server.initialState
  putStrLn ""

  forM_ (zip3 [1::Int ..] Server.calendar states') $ \(i, event, state) -> do
    putStrLn $ "Iteration " <> show i <> ":"
    putStrLn $ indent 4 $ "Event:"
    putStrLn $ indent 8 $ show event
    putStrLn $ indent 4 $ "State:"
    putStrLn $ indent 8 $ show state
    putStrLn ""

  putStrLn "Result:"
  putStrLn $ indent 4 $ show result
