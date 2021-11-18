{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | This module provides the 'Predicate' typeclass along with some built-in
-- predicates.
module Refined.Predicate
  ( -- * Typeclass
    Predicate (..),
    PC.mkRefineException,

    -- * Math
    NotEquals,
    NonZero,
    GreaterThanEq,
    NonNegative,
    GreaterThan,
    Positive,
    LessThanEq,
    NonPositive,
    LessThan,
    Negative,
    Even,
    Odd,

    -- * Foldable
    MaxLength,
    SortedAsc,
    SortedDesc,
  )
where

import Refined.Predicate.Class (Predicate (..))
import Refined.Predicate.Class qualified as PC
import Refined.Predicate.Foldable (MaxLength, SortedAsc, SortedDesc)
import Refined.Predicate.Math
  ( Even,
    GreaterThan,
    GreaterThanEq,
    LessThan,
    LessThanEq,
    Negative,
    NonNegative,
    NonPositive,
    NonZero,
    NotEquals,
    Odd,
    Positive,
  )