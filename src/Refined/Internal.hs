{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Internal module that exports types, including the unsafe
-- 'UnsafeRefined'.
module Refined.Internal
  ( Refined (MkRefined, ..),
    RefineException (..),
  )
where

import Data.Kind (Type)
import Data.Typeable (TypeRep)
import Language.Haskell.TH.Syntax (Lift (..))

-- | Newtype over @a@ that has a type-level list of attached predicates.
-- This is the core type for "Refined".
--
-- @since 0.1.0.0
type Refined :: [Type] -> Type -> Type
newtype Refined p a = UnsafeRefined
  { -- | @since 0.1.0.0
    unrefine :: a
  }
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Ord
    )
    via a
  deriving stock
    ( -- | @since 0.1.0.0
      Show
    )

type role Refined phantom nominal

-- | @since 0.1.0.0
instance (Lift x) => Lift (Refined p x) where
  liftTyped (UnsafeRefined a) = [||UnsafeRefined a||]

-- | @since 0.1.0.0
pattern MkRefined :: a -> Refined p a
pattern MkRefined a <- UnsafeRefined a

{-# COMPLETE MkRefined #-}

-- | 'RefineException' is used for errors encountered during refinement.
--
-- @since 0.1.0.0
data RefineException = MkRefineException
  { -- | Type rep for the type to which we tried to apply the refinement.
    --
    -- @since 0.1.0.0
    targetRep :: TypeRep,
    -- | Type rep for the predicate we tried to apply.
    --
    -- @since 0.1.0.0
    predRep :: TypeRep,
    -- | Description of the error encountered.
    --
    -- @since 0.1.0.0
    msg :: String
  }
  deriving stock
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Ord,
      -- | @since 0.1.0.0
      Show
    )