{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Provides predicates for numeric types.
--
-- @since 0.1.0.0
module Refined.Predicate.Nat
  ( -- * Equalities
    NatEq,
    NatNotEq,
    NonZero,

    -- * Inequalities
    NatGTEq,
    NonNegative,
    NatGT,
    Positive,
    NatLTEq,
    NonPositive,
    NatLT,
    Negative,

    -- * Misc
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
import Refined.Predicate.Operators (Not, type (\/))

-- | Predicate for @x = 'NatEq' n@ implies \(x = n \).
--
-- ==== __Examples__
-- >>> satisfies @(NatEq 5) Proxy 5
-- Nothing
--
-- >>> satisfies @(NatEq 5) Proxy 10
-- Just (MkRefineException {predRep = NatEq 5, targetRep = Integer, msg = "10 does not satisfy == 5"})
--
-- @since 0.1.0.0
type NatEq :: Nat -> Type
data NatEq n

-- | @since 0.1.0.0
instance forall n a. (KnownNat n, Num a, Ord a, Show a, Typeable a) => Predicate (NatEq n) a where
  satisfies _ x
    | x == n' = Nothing
    | otherwise = Just $ PC.mkRefineException @(NatEq n) @a err
    where
      n' = fromIntegral $ natVal' @n
      err = show x <> " does not satisfy == " <> show n'

-- | Predicate for @x = 'NatNotEq' n@ implies \(x \ne n \).
--
-- ==== __Examples__
-- >>> satisfies @(NatNotEq 5) Proxy 10
-- Nothing
--
-- >>> satisfies @(NatNotEq 5) Proxy 5
-- Just (MkRefineException {predRep = Not (NatEq 5), targetRep = Integer, msg = "5 does not satisfy (Not (NatEq 5))"})
--
-- @since 0.1.0.0
type NatNotEq :: Nat -> Type

type NatNotEq n = Not (NatEq n)

-- | 'NatNotEq' specialized to zero.
--
-- @since 0.1.0.0
type NonZero :: Type

type NonZero = NatNotEq 0

-- | Predicate for @x = 'NatGTEq' n@ implies \(x \geq n \).
--
-- ==== __Examples__
-- >>> satisfies @(NatGTEq 5) Proxy 5
-- Nothing
--
-- >>> satisfies @(NatGTEq 5) Proxy 4
-- Just (MkRefineException {predRep = Or (NatGT 5) (NatEq 5), targetRep = Integer, msg = "4 satisfied neither predicate"})
--
-- @since 0.1.0.0
type NatGTEq :: Nat -> Type

type NatGTEq n = NatGT n \/ NatEq n

-- | 'NatGTEq' specialized to 0.
--
-- @since 0.1.0.0
type NonNegative = NatGTEq 0

-- | Predicate for @x = 'NatGT' n@ implies \(x > n \).
--
-- ==== __Examples__
-- >>> satisfies @(NatGT 5) Proxy 6
-- Nothing
--
-- >>> satisfies @(NatGT 5) Proxy 5
-- Just (MkRefineException {predRep = NatGT 5, targetRep = Integer, msg = "5 does not satisfy > 5"})
--
-- @since 0.1.0.0
type NatGT :: Nat -> Type
data NatGT n

-- | @since 0.1.0.0
instance forall n a. (KnownNat n, Num a, Ord a, Show a, Typeable a) => Predicate (NatGT n) a where
  satisfies _ x
    | x > n' = Nothing
    | otherwise = Just $ PC.mkRefineException @(NatGT n) @a err
    where
      n' = fromIntegral $ natVal' @n
      err = show x <> " does not satisfy > " <> show n'

-- | 'NatGT' specialized to 0.
--
-- @since 0.1.0.0
type Positive = NatGT 0

-- | Predicate for @x = 'NatLTEq' n@ implies \(x \leq n \).
--
-- ==== __Examples__
-- >>> satisfies @(NatLTEq 5) Proxy 5
-- Nothing
--
-- >>> satisfies @(NatLTEq 5) Proxy 6
-- Just (MkRefineException {predRep = Or (NatLT 5) (NatEq 5), targetRep = Integer, msg = "6 satisfied neither predicate"})
--
-- @since 0.1.0.0
type NatLTEq :: Nat -> Type

type NatLTEq n = NatLT n \/ NatEq n

-- | Predicate for @x = 'NatLT' n@ implies \(x < n \).
--
-- ==== __Examples__
-- >>> satisfies @(NatLT 5) Proxy 4
-- Nothing
--
-- >>> satisfies @(NatLT 5) Proxy 5
-- Just (MkRefineException {predRep = NatLT 5, targetRep = Integer, msg = "5 does not satisfy < 5"})
--
-- @since 0.1.0.0
type NatLT :: Nat -> Type
data NatLT n

-- | @since 0.1.0.0
instance
  forall n a.
  (KnownNat n, Num a, Ord a, Show a, Typeable a) =>
  Predicate (NatLT n) a
  where
  satisfies _ x
    | x < n' = Nothing
    | otherwise = Just $ PC.mkRefineException @(NatLT n) @a err
    where
      n' = fromIntegral $ natVal' @n
      err = show x <> " does not satisfy < " <> show n'

-- | Predicate for non-positive types, i.e. \(n \leq 0\).
--
-- ==== __Examples__
-- >>> satisfies @NonPositive Proxy 0
-- Nothing
--
-- >>> satisfies @NonPositive Proxy 1
-- Just (MkRefineException {predRep = NonPositive, targetRep = Integer, msg = "1 does not satisfy <= 0"})
--
-- @since 0.1.0.0
data NonPositive

-- | @since 0.1.0.0
instance
  forall a.
  (Num a, Ord a, Show a, Typeable a) =>
  Predicate NonPositive a
  where
  satisfies _ x
    | x <= 0 = Nothing
    | otherwise = Just $ PC.mkRefineException @NonPositive @a err
    where
      err = show x <> " does not satisfy <= 0"

-- | Predicate for non-positive types, i.e. \(n < 0\).
--
-- ==== __Examples__
-- >>> satisfies @Negative Proxy (-2)
-- Nothing
--
-- >>> satisfies @Negative Proxy 0
-- Just (MkRefineException {predRep = Negative, targetRep = Integer, msg = "0 does not satisfy < 0"})
--
-- @since 0.1.0.0
data Negative

-- | @since 0.1.0.0
instance
  forall a.
  (Num a, Ord a, Show a, Typeable a) =>
  Predicate Negative a
  where
  satisfies _ x
    | x < 0 = Nothing
    | otherwise = Just $ PC.mkRefineException @Negative @a err
    where
      err = show x <> " does not satisfy < 0"

-- | Predicate for even integral types.
--
-- ==== __Examples__
-- >>> satisfies @Even Proxy 4
-- Nothing
--
-- >>> satisfies @Even Proxy 7
-- Just (MkRefineException {predRep = Even, targetRep = Integer, msg = "7 is not even"})
--
-- @since 0.1.0.0
type Even :: Type
data Even

-- | @since 0.1.0.0
instance forall a. (Integral a, Show a, Typeable a) => Predicate Even a where
  satisfies _ x
    | even x = Nothing
    | otherwise = Just $ PC.mkRefineException @Even @a err
    where
      err = show x <> " is not even"

-- | Predicate for odd integral types.
--
-- ==== __Examples__
-- >>> satisfies @Odd Proxy 5
-- Nothing
--
-- >>> satisfies @Odd Proxy 4
-- Just (MkRefineException {predRep = Odd, targetRep = Integer, msg = "4 is not odd"})
--
-- @since 0.1.0.0
type Odd :: Type
data Odd

-- | @since 0.1.0.0
instance forall a. (Integral a, Show a, Typeable a) => Predicate Odd a where
  satisfies _ x
    | odd x = Nothing
    | otherwise = Just $ PC.mkRefineException @Odd @a err
    where
      err = show x <> " is not odd"

natVal' :: forall n. KnownNat n => Integer
natVal' = toInteger $ TN.natVal (Proxy @n)
