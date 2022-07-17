module Client where

import Control.Monad.State.Lazy (State, get, put, evalState)

newtype Client = Client Int
  deriving (Eq, Ord, Show)

type ClientIdS = State Client

next :: ClientIdS Client
next = do
  c@(Client n) <- get
  put $ Client $ n + 1
  return c

evalS :: ClientIdS a -> Client -> a
evalS = evalState 
