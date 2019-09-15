module Caboodle.Generators where

import Caboodle hiding (finiteConsList)
import Hedgehog
import Optics

import qualified Caboodle as C

import qualified Hedgehog.Gen as Gen

finiteConsList :: Range Int -> Gen element -> Gen (FiniteConsList element)
finiteConsList sizeRange genElement = mustBeFinite <$> Gen.list sizeRange genElement

fingerList :: Range Int -> Gen element -> Gen (FingerList element)
fingerList sizeRange genElement = review C.finiteConsList <$> finiteConsList sizeRange genElement
