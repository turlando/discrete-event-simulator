module Simulator.Random where

import System.Random (RandomGen, uniformR)
import System.Random.Stateful (StatefulGen, uniformFloatPositive01M)

uniformFloatPositive01 :: RandomGen g => g -> (Float, g) 
uniformFloatPositive01 g = uniformR (0 :: Float, 1 :: Float) g

exponential :: RandomGen g => g -> Float -> (Float, g)
exponential g lambda
  = let (x, g') = uniformFloatPositive01 g
    in (- log x / lambda, g')

exponentialM :: StatefulGen g m => g -> Float -> m Float
exponentialM g lambda = (\x -> - log x / lambda) <$> (uniformFloatPositive01M g) 
