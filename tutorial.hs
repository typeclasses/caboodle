{-# LANGUAGE LambdaCase, OverloadedLists, OverloadedStrings, ScopedTypeVariables, TypeApplications #-}

module Caboodle.Tutorial where

import Caboodle
import Hedgehog hiding (discard)
import Optics
import Numeric.Natural
import Prelude hiding (filter)

import qualified Hedgehog.Gen as Gen
import qualified Caboodle.Generators as Gen

import qualified Hedgehog.Range as Range

tutorial =
  do

    do
      let numbers :: FingerList Natural = [4,1,5,7,8,1,4]
      [1,4,5,7,8] === sortUnique numbers
      [4,8,4] === keep even numbers
      [4,8,4] === discard odd numbers
      [1,5,7,1] === keep odd numbers
      [1,5,7,1] === discard even numbers

    do
      numbers <- forAll $ Gen.fingerList (Range.linear 0 10) (Gen.integral (Range.linear @Natural 0 100))
      keep even numbers === discard odd numbers
      keep odd numbers === discard even numbers

    do
      let text :: FiniteConsText = "hello"
      "ehlo" === sortUnique text
      "heo" === filter (\case 'l' -> Discard; _ -> Keep) text

testTutorial :: IO Bool
testTutorial = checkParallel (Group "Caboodle" [("tutorial", property tutorial)])
