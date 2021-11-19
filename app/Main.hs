module Main where

import Text.Pretty.Simple (pPrint)

import qualified Simulator
import qualified Simulator.Example.Server as Server

main :: IO ()
main = do
  let states = Simulator.states Server.initialState Server.calendar
      result = Simulator.result $ last states
  pPrint states
  pPrint result
