-- | This module provides the 'Predicate' typeclass along with some built-in
-- predicates.
module Refined.Predicate
  ( -- * Typeclass
    Predicate (..),
    PC.mkRefineException,

    -- * Built-in Predicates
    module Refined.Predicate.Math,
    module Refined.Predicate.Foldable,
    module Refined.Predicate.Text,
  )
where

import Refined.Predicate.Class (Predicate (..))
import Refined.Predicate.Class qualified as PC
import Refined.Predicate.Foldable
import Refined.Predicate.Math
import Refined.Predicate.Text
