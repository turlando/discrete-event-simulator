module Simulator.CalendarSpec where

import Test.Hspec (Spec, describe, it, shouldBe)

import Simulator.Calendar (Calendar(Empty, (:^)))
import qualified Simulator.Calendar as C
import qualified Simulator.Instant as I

cal :: Calendar Char
cal = C.fromList
      [ (I.fromInt 3, 'a')
      , (I.fromInt 1, 'b')
      , (I.fromInt 2, 'c')
      ]

spec :: Spec
spec = do
  describe "Pattern matching" $ do
    it "can match Empty" $ do
      let empty = case C.empty of
            Empty -> True
            _     -> False
      empty `shouldBe` True
    it "can match first element" $ do
      let b = case cal of
            x :^ _xs -> x
            _        -> (9, 'z')
      b `shouldBe` (I.fromInt 1, 'b')
