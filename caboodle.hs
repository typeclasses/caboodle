{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE FlexibleInstances, FunctionalDependencies, LambdaCase, NoImplicitPrelude, TypeFamilies #-}

module Caboodle
  (
  -- * Aspects of caboodles
  -- ** Elements (of sets and sequences)
  -- $elements
    Element
  -- ** Keys and values (of maps)
  -- $keysAndValues
  , Key, Value
  -- ** Size and counting
  , Size
  -- *** Finite size
  , Finite (..)
  -- *** Counting with an upper bound
  , Countable (..), Count (..), Limit, countExactly
  -- ** Tips
  , Tip (..), tipBase
  -- *** Endpoints (left/right sides)
  , Endpoints (..), Side (..)
  -- *** Extrema (min/max)
  , Extreme (..), Extrema (..)
  -- ** Filtering
  , MapFilter (..), Judgement (..)
  -- ** Triviality
  , Trivial (..), Triviality (..), zero, one, empty, null, singleton
  -- ** Testing for membership
  , Membership (..), (∈)
  -- ** Insertion
  , Insert (..)
  -- ** Deletion
  , Delete (..)
  -- ** Convertion with list
  , List (..), fromList, toList
  -- ** Set operations
  , Intersection (..), Union (..)
  -- ** Map operations
  , MapEnumerable (..), MapIntersection (..), MapUnion (..), TotalMap (..)

  -- * Caboodles
  -- ** Sequences
  , FingerSeq
  -- ** Sets
  , OrdSet, HashSet
  -- ** Maps
  , OrdMap, HashMap

  ) where

import Prelude ((.))
import Prelude (Bool (..), Eq, Show, Maybe (..), Ord (..))
import Prelude (fromIntegral)

import Control.Monad ((>=>))
import Numeric.Natural (Natural)

import qualified Data.Maybe as Maybe
import qualified Data.Foldable as Foldable

-- optics
import Optics

-- containers
import qualified Data.Map.Lazy as OrdMap
import qualified Data.Set as OrdSet
import qualified Data.Sequence as FingerSeq

-- unordered-containers
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.HashSet as HashSet


--- Type families ---

-- $elements
-- Sets and sequences have an 'Element' type.

-- $keysAndValues
-- Maps have a 'Key' type and a 'Value' type.

type family Element caboodle

type family Key caboodle

type family Value caboodle


--- Types ---

data Count = Counted Size | TooMany

data Extreme = Low | High

data Judgement = Keep | Discard

type Limit = Size

data Side = Left | Right

type Size = Natural

data Tip element caboodle = Nil | element :+ caboodle
  deriving (Eq, Show)

data Triviality element =
    Zero
      -- ^ An empty collection.
  | One element
      -- ^ A collection with exactly one item.
  deriving (Eq, Show)


--- Classes ---

-- | A 'Countable' caboodle is one whose size we can attempt to determine by counting its elements.

class Countable caboodle
  where
    -- | Since a caboodle is potentially infinite in size, 'count' has a 'Limit' parameter. If the counting exceeds this limit, then the caboodle gives up and returns 'TooMany'.
    count :: Limit -> caboodle -> Count

class Delete caboodle
  where
    delete :: Element caboodle -> caboodle -> caboodle

class Endpoints caboodle
  where
    side :: Side -> Iso' caboodle (Tip (Element caboodle) caboodle)

class Extrema caboodle
  where
    extreme :: Extreme -> caboodle -> Tip (Element caboodle) caboodle

class Finite caboodle
  where
    size :: caboodle -> Size

class Insert caboodle
  where
    insert :: Element caboodle -> caboodle -> caboodle

class Intersection caboodle
  where
    intersection :: caboodle -> caboodle -> caboodle

class List caboodle
  where
    list :: Iso' caboodle [Element caboodle]

class MapEnumerable caboodle
  where
    enumerateKeys :: caboodle -> [Key caboodle]
    enumerateValues :: caboodle -> [Value caboodle]

class MapFilter caboodle
  where
    filterValues :: (Value caboodle -> Judgement) -> caboodle -> caboodle

class MapIntersection caboodle
  where
    mapIntersection ::
        (Value caboodle -> Value caboodle -> Value caboodle)
        -> caboodle -> caboodle -> caboodle

class MapUnion caboodle
  where
    mapUnion ::
        (Value caboodle -> Value caboodle -> Value caboodle)
        -> caboodle -> caboodle -> caboodle

class Membership caboodle
  where
    member :: Element caboodle -> caboodle -> Bool

class TotalMap caboodle
  where
    att :: key -> Lens' caboodle (Value caboodle)

class Trivial caboodle
  where
    triviality :: Prism' caboodle (Triviality (Element caboodle))

class Union caboodle
  where
    union :: caboodle -> caboodle -> caboodle


--- Functions ---

countExactly :: Finite caboodle => Limit -> caboodle -> Count
countExactly limit caboodle =
    if (size caboodle <= limit)
        then Counted (size caboodle)
        else TooMany

-- | An alias for 'member' used commonly <https://en.wikipedia.org/wiki/Element_(mathematics)#Notation_and_terminology in the notation of set theory>.
(∈) :: Membership caboodle => Element caboodle -> caboodle -> Bool
(∈) = member

null :: Trivial caboodle => caboodle -> Bool
null = Maybe.isJust . preview zero

empty :: Trivial caboodle => caboodle
empty = review zero ()

singleton :: Trivial caboodle => Element caboodle -> caboodle
singleton = review one

toList :: List caboodle => caboodle -> [Element caboodle]
toList = view list

fromList :: List caboodle => [Element caboodle] -> caboodle
fromList = review list


--- Optics ---

zero :: Trivial caboodle => Prism' caboodle ()
zero =
  prism'
    (\() -> review triviality Zero)
    (preview triviality >=> \case Zero -> Just (); _ -> Nothing)

one :: Trivial caboodle => Prism' caboodle (Element caboodle)
one =
  prism'
    (\x -> review triviality (One x))
    (preview triviality >=> \case One x -> Just x; _ -> Nothing)

-- | Isomorphism between the 'Tip' type defined in "Caboodle" and the corresponding representation of tips in modules such as "Data.Set" using types in the @base@ package.

tipBase :: Iso' (Tip (Element caboodle) caboodle) (Maybe (Element caboodle, caboodle))
tipBase = iso f g
  where
    f = \case Nil -> Nothing; x :+ xs -> Just (x, xs)
    g = \case Nothing -> Nil; Just (x, xs) -> x :+ xs


--- FingerSeq ---

type FingerSeq element = FingerSeq.Seq element

type instance Element (FingerSeq element) = element

instance Countable (FingerSeq element)
  where
    count = countExactly

instance Endpoints (FingerSeq element)
  where
    side Left = iso (f . FingerSeq.viewl) g
      where
        f = \case FingerSeq.EmptyL -> Nil; x FingerSeq.:< xs -> x :+ xs
        g = \case Nil -> FingerSeq.empty; x :+ xs -> x FingerSeq.<| xs
    side Right = iso (f . FingerSeq.viewr) g
      where
        f = \case FingerSeq.EmptyR -> Nil; xs FingerSeq.:> x -> x :+ xs
        g = \case Nil -> FingerSeq.empty; x :+ xs -> xs FingerSeq.|> x

instance Finite (FingerSeq element)
  where
    size = fromIntegral . FingerSeq.length

instance List (FingerSeq element)
  where
    list = iso Foldable.toList FingerSeq.fromList

instance Trivial (FingerSeq element)
  where
    triviality = prism' f (g . Foldable.toList)
      where
        f = \case Zero -> FingerSeq.empty; One x -> FingerSeq.singleton x
        g = \case [] -> Just Zero; [x] -> Just (One x); _ -> Nothing


--- OrdSet ---

type OrdSet element = OrdSet.Set element

type instance Element (OrdSet element) = element
type instance Key (OrdSet element) = element
type instance Value (OrdSet element) = ()

instance Countable (OrdSet element)
  where
    count = countExactly

instance Ord element => Delete (OrdSet element)
  where
    delete = OrdSet.delete

instance Ord element => Extrema (OrdSet element)
  where
    extreme ex = review tipBase . f ex
      where
        f = \case Low -> OrdSet.minView; High -> OrdSet.maxView

instance Finite (OrdSet element)
  where
    size = fromIntegral . OrdSet.size

instance Ord element => Insert (OrdSet element)
  where
    insert = OrdSet.insert

instance Ord element => Intersection (OrdSet element)
  where
    intersection = OrdSet.intersection

instance Ord element => List (OrdSet element)
  where
    list = iso Foldable.toList OrdSet.fromList

instance Ord element => Membership (OrdSet element)
  where
    member = OrdSet.member

instance Ord element => Trivial (OrdSet element)
  where
    triviality = prism' f (g . Foldable.toList)
      where
        f = \case Zero -> OrdSet.empty; One x -> OrdSet.singleton x
        g = \case [] -> Just Zero; [x] -> Just (One x); _ -> Nothing

instance Ord element => Union (OrdSet element)
  where
    union = OrdSet.union


--- OrdMap ---

type OrdMap key value = OrdMap.Map key value

type instance Element (OrdMap key value) = (key, value)
type instance Key (OrdMap key value) = key
type instance Value (OrdMap key value) = value

instance Countable (OrdMap key value)
  where
    count = countExactly

instance Ord key => Extrema (OrdMap key value)
  where
    extreme ex = review tipBase . f ex
      where
        f = \case Low -> OrdMap.minViewWithKey; High -> OrdMap.maxViewWithKey

instance Finite (OrdMap key value)
  where
    size = fromIntegral . OrdMap.size

instance Ord key => Insert (OrdMap key value)
  where
    insert (k, a) = OrdMap.insert k a

instance Ord key => List (OrdMap key value)
  where
    list = iso OrdMap.toList OrdMap.fromList

instance MapEnumerable (OrdMap key value)
  where
    enumerateKeys = OrdMap.keys
    enumerateValues = Foldable.toList

instance Ord key => MapFilter (OrdMap key value)
  where
    filterValues f = OrdMap.filter ((\case Keep -> True; Discard -> False) . f)

instance Ord key => MapIntersection (OrdMap key value)
  where
    mapIntersection = OrdMap.intersectionWith

instance Ord key => MapUnion (OrdMap key value)
  where
    mapUnion = OrdMap.unionWith

instance Trivial (OrdMap key value)
  where
    triviality = prism' f (g . OrdMap.toList)
      where
        f = \case Zero -> OrdMap.empty; One (k, a) -> OrdMap.singleton k a
        g = \case [] -> Just Zero; [x] -> Just (One x); _ -> Nothing


--- HashSet ---

type HashSet element = HashSet.HashSet element


--- HashMap ---

type HashMap key value = HashMap.HashMap key value
