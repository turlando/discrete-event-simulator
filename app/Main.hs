module Main where

import Data.List (intercalate)
import Control.Monad (forM_)
import Simulator.Frame (Frame(Frame))

import qualified Simulator
import qualified Simulator.Example.Server as Server

indent :: Int -> String -> String
indent spaces string
  = intercalate "\n" $ map (\l -> replicate spaces ' ' <> l) (lines string)

main :: IO ()
main = do
  let (result, frames) = Simulator.run Server.initialState Server.calendar

  putStrLn "Initial state:"
  putStrLn $ indent 4 $ show Server.initialState
  putStrLn ""

  forM_ (zip [1::Int ..] frames) $ \(i, (Frame time event state)) -> do
    putStrLn $ "Iteration " <> show i <> " / " <> show time <> ":"
    putStrLn $ indent 4 $ "Event:"
    putStrLn $ indent 8 $ show event
    putStrLn $ indent 4 $ "State:"
    putStrLn $ indent 8 $ show state
    putStrLn ""

  putStrLn "Result:"
  putStrLn $ indent 4 $ show result
