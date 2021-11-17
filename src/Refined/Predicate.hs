{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | This module provides the 'Predicate' typeclass along with some built-in
-- predicates.
module Refined.Predicate
  ( -- * Typeclass
    Predicate (..),
    mkRefineException,

    -- * General Predicates
    NotEquals,
    GreaterThanEq,
    GreaterThan,
    LessThanEq,
    LessThan,
    Even,
    Odd,

    -- * Specializations
    NonZero,
    NonNegative,
    Positive,
    NonPositive,
    Negative,
  )
where

import Data.Kind (Type)
import Data.Typeable (Proxy (..), Typeable)
import Data.Typeable qualified as Ty
import GHC.TypeNats (KnownNat, Nat)
import GHC.TypeNats qualified as TN
import Refined.Internal (RefineException (..))

-- | This class is used for defining new predicates.
--
-- @since 0.1.0.0
class (Typeable p, Typeable a) => Predicate p a where
  -- | Validates predicate @p@ for the type @a@. Returns 'Nothing' on success,
  -- 'Just' 'RefineException' on a validation failure.
  --
  -- @since 0.1.0.0
  validate :: Proxy p -> a -> Maybe RefineException

-- | @mkRefineException \@p \@a msg@ creates a 'RefineException' for type @a@,
-- predicate @p@, and error message @msg@.
--
-- @since 0.1.0.0
mkRefineException :: forall p a. Predicate p a => String -> RefineException
mkRefineException = MkRefineException pTy aTy
  where
    pTy = Ty.typeRep (Proxy @p)
    aTy = Ty.typeRep (Proxy @a)

-- | Predicate for @x = 'NotEquals' n@ implies \(x \ne n \).
--
-- @since 0.1.0.0
type NotEquals :: Nat -> Type
data NotEquals n

-- | 'NotEquals' specialized to zero.
--
-- @since 0.1.0.0
type NonZero :: Type

type NonZero = NotEquals 0

-- | @since 0.1.0.0
instance forall n a. (KnownNat n, Num a, Ord a, Show a, Typeable a) => Predicate (NotEquals n) a where
  validate _ x
    | x /= n' = Nothing
    | otherwise = Just $ mkRefineException @(NotEquals n) @a err
    where
      n' = fromIntegral $ natVal' @n
      err = show x <> " does not satisfy /= " <> show n'

-- | Predicate for @x = 'GreaterThanEq' n@ implies \(x \geq n \).
--
-- @since 0.1.0.0
type GreaterThanEq :: Nat -> Type
data GreaterThanEq n

-- | 'GreaterThanEq' specialized to 0.
--
-- @since 0.1.0.0
type NonNegative = GreaterThanEq 0

-- | @since 0.1.0.0
instance forall n a. (KnownNat n, Num a, Ord a, Show a, Typeable a) => Predicate (GreaterThanEq n) a where
  validate _ x
    | x >= n' = Nothing
    | otherwise = Just $ mkRefineException @(GreaterThanEq n) @a err
    where
      n' = fromIntegral $ natVal' @n
      err = show x <> " does not satisfy >= " <> show n'

-- | Predicate for @x = 'GreaterThan' n@ implies \(x > n \).
--
-- @since 0.1.0.0
type GreaterThan :: Nat -> Type
data GreaterThan n

-- | @since 0.1.0.0
instance forall n a. (KnownNat n, Num a, Ord a, Show a, Typeable a) => Predicate (GreaterThan n) a where
  validate _ x
    | x > n' = Nothing
    | otherwise = Just $ mkRefineException @(GreaterThan n) @a err
    where
      n' = fromIntegral $ natVal' @n
      err = show x <> " does not satisfy > " <> show n'

-- | 'GreaterThan' specialized to 0.
--
-- @since 0.1.0.0
type Positive = GreaterThan 0

-- | Predicate for @x = 'LessThanEq' n@ implies \(x \leq n \).
--
-- @since 0.1.0.0
type LessThanEq :: Nat -> Type
data LessThanEq n

-- | @since 0.1.0.0
instance forall n a. (KnownNat n, Num a, Ord a, Show a, Typeable a) => Predicate (LessThanEq n) a where
  validate _ x
    | x <= n' = Nothing
    | otherwise = Just $ mkRefineException @(LessThanEq n) @a err
    where
      n' = fromIntegral $ natVal' @n
      err = show x <> " does not satisfy <= " <> show n'

-- | Predicate for @x = 'LessThan' n@ implies \(x < n \).
--
-- @since 0.1.0.0
type LessThan :: Nat -> Type
data LessThan n

-- | @since 0.1.0.0
instance forall n a. (KnownNat n, Num a, Ord a, Show a, Typeable a) => Predicate (LessThan n) a where
  validate _ x
    | x < n' = Nothing
    | otherwise = Just $ mkRefineException @(LessThan n) @a err
    where
      n' = fromIntegral $ natVal' @n
      err = show x <> " does not satisfy < " <> show n'

-- | Predicate for non-positive types, i.e. \(n \leq 0\).
--
-- @since 0.1.0.0
data NonPositive

-- | @since 0.1.0.0
instance forall a. (Num a, Ord a, Show a, Typeable a) => Predicate NonPositive a where
  validate _ x
    | x <= 0 = Nothing
    | otherwise = Just $ mkRefineException @NonPositive @a err
    where
      err = show x <> " does not satisfy <= 0"

-- | Predicate for non-positive types, i.e. \(n < 0\).
--
-- @since 0.1.0.0
data Negative

-- | @since 0.1.0.0
instance forall a. (Num a, Ord a, Show a, Typeable a) => Predicate Negative a where
  validate _ x
    | x < 0 = Nothing
    | otherwise = Just $ mkRefineException @Negative @a err
    where
      err = show x <> " does not satisfy < 0"

-- | Predicate for even integral types.
--
-- @since 0.1.0.0
type Even :: Type
data Even

-- | @since 0.1.0.0
instance forall a. (Integral a, Show a, Typeable a) => Predicate Even a where
  validate _ x
    | even x = Nothing
    | otherwise = Just $ mkRefineException @Even @a err
    where
      err = show x <> " is not even"

-- | Predicate for odd integral types.
--
-- @since 0.1.0.0
type Odd :: Type
data Odd

-- | @since 0.1.0.0
instance forall a. (Integral a, Show a, Typeable a) => Predicate Odd a where
  validate _ x
    | odd x = Nothing
    | otherwise = Just $ mkRefineException @Odd @a err
    where
      err = show x <> " is not odd"

natVal' :: forall n. KnownNat n => Integer
natVal' = toInteger $ TN.natVal (Proxy @n)
