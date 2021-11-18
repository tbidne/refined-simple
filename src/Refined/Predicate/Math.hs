{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Provides predicates for numeric types.
module Refined.Predicate.Math
  ( NotEquals,
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
  )
where

import Data.Kind (Type)
import Data.Typeable (Proxy (..), Typeable)
import GHC.TypeNats (KnownNat, Nat)
import GHC.TypeNats qualified as TN
import Refined.Predicate.Class (Predicate (..))
import Refined.Predicate.Class qualified as PC

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
    | otherwise = Just $ PC.mkRefineException @(NotEquals n) @a err
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
    | otherwise = Just $ PC.mkRefineException @(GreaterThanEq n) @a err
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
    | otherwise = Just $ PC.mkRefineException @(GreaterThan n) @a err
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
instance
  forall n a.
  (KnownNat n, Num a, Ord a, Show a, Typeable a) =>
  Predicate (LessThanEq n) a
  where
  validate _ x
    | x <= n' = Nothing
    | otherwise = Just $ PC.mkRefineException @(LessThanEq n) @a err
    where
      n' = fromIntegral $ natVal' @n
      err = show x <> " does not satisfy <= " <> show n'

-- | Predicate for @x = 'LessThan' n@ implies \(x < n \).
--
-- @since 0.1.0.0
type LessThan :: Nat -> Type
data LessThan n

-- | @since 0.1.0.0
instance
  forall n a.
  (KnownNat n, Num a, Ord a, Show a, Typeable a) =>
  Predicate (LessThan n) a
  where
  validate _ x
    | x < n' = Nothing
    | otherwise = Just $ PC.mkRefineException @(LessThan n) @a err
    where
      n' = fromIntegral $ natVal' @n
      err = show x <> " does not satisfy < " <> show n'

-- | Predicate for non-positive types, i.e. \(n \leq 0\).
--
-- @since 0.1.0.0
data NonPositive

-- | @since 0.1.0.0
instance
  forall a.
  (Num a, Ord a, Show a, Typeable a) =>
  Predicate NonPositive a
  where
  validate _ x
    | x <= 0 = Nothing
    | otherwise = Just $ PC.mkRefineException @NonPositive @a err
    where
      err = show x <> " does not satisfy <= 0"

-- | Predicate for non-positive types, i.e. \(n < 0\).
--
-- @since 0.1.0.0
data Negative

-- | @since 0.1.0.0
instance
  forall a.
  (Num a, Ord a, Show a, Typeable a) =>
  Predicate Negative a
  where
  validate _ x
    | x < 0 = Nothing
    | otherwise = Just $ PC.mkRefineException @Negative @a err
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
    | otherwise = Just $ PC.mkRefineException @Even @a err
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
    | otherwise = Just $ PC.mkRefineException @Odd @a err
    where
      err = show x <> " is not odd"

natVal' :: forall n. KnownNat n => Integer
natVal' = toInteger $ TN.natVal (Proxy @n)
