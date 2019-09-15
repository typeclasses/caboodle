{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE BangPatterns, ConstraintKinds, FlexibleContexts, FlexibleInstances, FunctionalDependencies, LambdaCase, NoImplicitPrelude, ScopedTypeVariables, TypeFamilies #-}

module Caboodle
  (
  -- * Aspects of caboodles
  -- ** Elements (of sets and lists)
  -- $elements
    Element
  -- ** Keys and values (of maps)
  -- $keysAndValues
  , Key, Value
  -- ** Size and counting
  , Size
  -- *** Finite size
  , Finite (..), mustBeFinite
  -- *** Counting with an upper bound
  , Countable (..), Count (..), Limit, countExactly
  -- ** Tips
  , Tip (..), tipBase
  -- *** Endpoints (left/right sides)
  , Endpoints (..), Side (..)
  -- *** Extrema (min/max)
  , Extreme (..), Extrema (..)
  -- ** Filtering
  , Filter (..), MapFilter (..), Judgement (..), keepJudgement, keep, discard
  -- ** Triviality
  , Trivial (..), Triviality (..), zero, one, empty, null, singleton
  -- ** Testing for membership
  , Membership (..), (∈)
  -- ** Insertion
  , Insert (..)
  -- ** Deletion
  , Delete (..)
  -- ** Set operations
  , Intersection (..), Union (..)
  -- ** Map operations
  , MapEnumerable (..), MapIntersection (..), MapUnion (..), TotalMap (..)
  -- ** Sorting
  , sortUnique
  -- ** Conversions between caboodle types
  , ToConsList (..), FiniteConsListIso (..), convertViaFiniteConsList

  -- * Caboodles
  -- ** Lists
  , FingerList, ConsList, FiniteConsList, FiniteConsText
  -- ** Sets
  , OrdSet, HashSet
  -- ** Maps
  , OrdMap, HashMap

  -- * Bundles of constraints
  , Map

  ) where

import Prelude ((.), succ)
import Prelude (Bool (..), Eq (..), Show, Maybe (..), Ord (..), Char)
import Prelude (fromIntegral)

import Control.Monad ((>=>))
import Numeric.Natural (Natural)

import qualified Data.List as ConsList
import qualified Data.Maybe as Maybe
import qualified Data.Foldable as Foldable

import qualified GHC.Exts

-- optics
import Optics

-- containers
import qualified Data.Map.Lazy as OrdMap
import qualified Data.Set as OrdSet
import qualified Data.Sequence as FingerList

-- unordered-containers
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.HashSet as HashSet


--- Type families ---

-- $elements
-- Sets and lists have an 'Element' type.

type family Element caboodle

-- $keysAndValues
-- Maps have a 'Key' type and a 'Value' type.

type family Key caboodle

type family Value caboodle


--- Types ---

data Count = Counted Size | TooMany
  deriving (Eq, Show)

data Extreme = Low | High
  deriving (Eq, Show)

data Judgement = Keep | Discard
  deriving (Eq, Show)

type Limit = Size

-- | Finite lists have two ends which we call 'Left' and 'Right'.
--
-- ==== Meaning of "left" and "right"
--
-- Which end receives which of these two labels is somewhat arbitrary but usually corresponds to how we write the list. For example, in the string @hello@, the "leftmost" character is @h@ and the "rightmost" is @o@.
--
-- ==== Relationship to "beginning" and "end"
--
-- Sometimes we think of 'Left' as the "beginning" and 'Right' as the "end", since we write from left to right and traverse list types such as 'FingerList' from left to right. But this left-to-right convention is not a /necessary/ aspect of lists.

data Side = Left | Right
  deriving (Eq, Show)

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

class Filter caboodle
  where
    filter :: (Element caboodle -> Judgement) -> caboodle -> caboodle

class Finite caboodle
  where
    size :: caboodle -> Size

class FiniteConsListIso caboodle
  where
    finiteConsList :: Iso' caboodle (FiniteConsList (Element caboodle))

class Insert caboodle
  where
    insert :: Element caboodle -> caboodle -> caboodle

class Intersection caboodle
  where
    intersection :: caboodle -> caboodle -> caboodle

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

class ToConsList caboodle
  where
    toConsList :: caboodle -> ConsList (Element caboodle)

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

convertViaFiniteConsList :: (FiniteConsListIso caboodle1, FiniteConsListIso caboodle2, Element caboodle1 ~ Element caboodle2) => caboodle1 -> caboodle2
convertViaFiniteConsList = review finiteConsList . view finiteConsList

sortUnique :: forall caboodle. (FiniteConsListIso caboodle, Ord (Element caboodle)) => caboodle -> caboodle
sortUnique c = convertViaFiniteConsList (convertViaFiniteConsList c :: OrdSet (Element caboodle))

keep :: Filter caboodle => (Element caboodle -> Bool) -> caboodle -> caboodle
keep f = filter (review keepJudgement . f)

discard :: Filter caboodle => (Element caboodle -> Bool) -> caboodle -> caboodle
discard f = filter (review discardJudgement . f)

-- | Use this conversion only when you have a cons list that you know to be finite.

mustBeFinite :: ConsList element -> FiniteConsList element
mustBeFinite = FiniteConsList


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

keepJudgement :: Iso' Judgement Bool
keepJudgement = iso (\case Keep -> True; Discard -> False) (\case True -> Keep; False -> Discard)

discardJudgement :: Iso' Judgement Bool
discardJudgement = iso (\case Keep -> False; Discard -> True) (\case True -> Discard; False -> Keep)


--- ConsList ---

type ConsList element = [element]

type instance Element (ConsList element) = element

instance Countable (ConsList element)
  where
    count limit = go 0
      where
        go c =
          \case
            []                  ->  Counted c
            _ : _ | c == limit  ->  TooMany
            _ : xs              ->  let !c' = succ c in go c' xs

instance Filter (ConsList element)
  where
    filter f = ConsList.filter (view keepJudgement . f)

instance ToConsList (ConsList element)
  where
    toConsList xs = xs


--- FiniteConsList ---

newtype FiniteConsList element = FiniteConsList (ConsList element)
  deriving (Eq, Show)

type instance Element (FiniteConsList element) = element

instance GHC.Exts.IsList (FiniteConsList element)
  where
    type Item (FiniteConsList element) = element
    toList (FiniteConsList xs) = xs
    fromList = mustBeFinite

instance Filter (FiniteConsList element)
  where
    filter f (FiniteConsList xs) = mustBeFinite (filter f xs)

instance FiniteConsListIso (FiniteConsList element)
  where
    finiteConsList = simple

instance ToConsList (FiniteConsList element)
  where
    toConsList (FiniteConsList xs) = xs


--- FiniteString ---

type FiniteConsText = FiniteConsList Char

instance GHC.Exts.IsString FiniteConsText
  where
    fromString = mustBeFinite


--- FingerList ---

type FingerList element = FingerList.Seq element

type instance Element (FingerList element) = element

instance Countable (FingerList element)
  where
    count = countExactly

instance Endpoints (FingerList element)
  where
    side Left = iso (f . FingerList.viewl) g
      where
        f = \case FingerList.EmptyL -> Nil; x FingerList.:< xs -> x :+ xs
        g = \case Nil -> FingerList.empty; x :+ xs -> x FingerList.<| xs
    side Right = iso (f . FingerList.viewr) g
      where
        f = \case FingerList.EmptyR -> Nil; xs FingerList.:> x -> x :+ xs
        g = \case Nil -> FingerList.empty; x :+ xs -> xs FingerList.|> x

instance Filter (FingerList element)
  where
    filter f = FingerList.filter (view keepJudgement . f)

instance Finite (FingerList element)
  where
    size = fromIntegral . FingerList.length

instance FiniteConsListIso (FingerList element)
  where
    finiteConsList = iso Foldable.toList FingerList.fromList % coerced

instance ToConsList (FingerList element)
  where
    toConsList = Foldable.toList

instance Trivial (FingerList element)
  where
    triviality = prism' f (g . Foldable.toList)
      where
        f = \case Zero -> FingerList.empty; One x -> FingerList.singleton x
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

instance Filter (OrdSet element)
  where
    filter f = OrdSet.filter (view keepJudgement . f)

instance Ord element => FiniteConsListIso (OrdSet element)
  where
    finiteConsList = iso Foldable.toList OrdSet.fromList % coerced

instance Ord element => Insert (OrdSet element)
  where
    insert = OrdSet.insert

instance Ord element => Intersection (OrdSet element)
  where
    intersection = OrdSet.intersection

instance Ord element => Membership (OrdSet element)
  where
    member = OrdSet.member

instance Ord element => ToConsList (OrdSet element)
  where
    toConsList = Foldable.toList

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

instance Filter (OrdMap key value)
  where
    filter f = OrdMap.filterWithKey (\key value -> view keepJudgement (f (key, value)))

instance Finite (OrdMap key value)
  where
    size = fromIntegral . OrdMap.size

instance Ord key => FiniteConsListIso (OrdMap key value)
  where
    finiteConsList = iso OrdMap.toList OrdMap.fromList % coerced

instance Ord key => Insert (OrdMap key value)
  where
    insert (k, a) = OrdMap.insert k a

instance Ord key => ToConsList (OrdMap key value)
  where
    toConsList = OrdMap.toList

instance MapEnumerable (OrdMap key value)
  where
    enumerateKeys = OrdMap.keys
    enumerateValues = Foldable.toList

instance Ord key => MapFilter (OrdMap key value)
  where
    filterValues f = OrdMap.filter (view keepJudgement . f)

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


--- Bundles of constraints ---

type Map key value caboodle =
  ( Key caboodle ~ key
  , Value caboodle ~ value
  , Countable caboodle
  , Extrema caboodle
  , Filter caboodle
  , Finite caboodle
  , FiniteConsListIso caboodle
  , Insert caboodle
  , ToConsList caboodle
  , MapEnumerable caboodle
  , MapFilter caboodle
  , MapIntersection caboodle
  , MapUnion caboodle
  , Trivial caboodle
  )
