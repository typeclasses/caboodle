{-# LANGUAGE OverloadedLists, OverloadedStrings, ScopedTypeVariables, TypeApplications #-}

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

      sortUnique numbers === [1,4,5,7,8]

      keep even numbers    === [4,8,4]
      discard odd numbers  === [4,8,4]

      keep odd numbers     === [1,5,7,1]
      discard even numbers === [1,5,7,1]

    do
      let g = Gen.fingerList (Range.linear 0 10)
              (Gen.integral (Range.linear @Natural 0 100))
      numbers <- forAll g
      keep even numbers === discard odd numbers
      keep odd numbers === discard even numbers

    do
      let text :: FiniteConsText = "hello"
      sortUnique text === "ehlo"

testTutorial :: IO Bool
testTutorial = checkParallel (Group "Caboodle" [("tutorial", property tutorial)])
