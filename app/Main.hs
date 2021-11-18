module Main where

import Text.Pretty.Simple (pPrint)

import qualified Simulator
import qualified Simulator.Example.Server as Server

main :: IO ()
main = pPrint
     $ Simulator.states Server.initialState Server.calendar
