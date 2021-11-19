{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Provides predicates for 'Foldable'.
module Refined.Predicate.Foldable
  ( MaxLength,
    MinLength,
    ExactLength,
    Increasing,
    StrictlyIncreasing,
    Decreasing,
    StrictlyDecreasing,
  )
where

import Data.Foldable qualified as F
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (Proxy (..), Typeable)
import GHC.TypeNats (KnownNat, Nat)
import GHC.TypeNats qualified as TN
import Refined.Predicate.Class (Predicate (..))
import Refined.Predicate.Class qualified as PC

-- | Predicate for maximum length.
--
-- >>> validate @(MaxLength 10) Proxy [1..10]
-- Nothing
--
-- >>> validate @(MaxLength 10) Proxy [1..11]
-- Just (MkRefineException {predRep = MaxLength 10, targetRep = [Integer], msg = "[1,2,3,4,5,6,7,8,9,10,11] does not satisfy length <= 10"})
--
-- @since 0.1.0.0
type MaxLength :: Nat -> Type
data MaxLength n

-- | @since 0.1.0.0
instance
  forall n f a.
  (Foldable f, KnownNat n, Typeable f, Show (f a), Typeable a) =>
  Predicate (MaxLength n) (f a)
  where
  validate _ xs
    | length xs <= len = Nothing
    | otherwise = Just $ PC.mkRefineException @(MaxLength n) @(f a) err
    where
      len = fromIntegral $ natVal' @n
      err = show xs <> " does not satisfy length <= " <> show len

instance KnownNat n => Predicate (MaxLength n) Text where
  validate _ txt
    | T.length txt <= len = Nothing
    | otherwise = Just $ PC.mkRefineException @(MaxLength n) @Text err
    where
      len = fromIntegral $ natVal' @n
      err = show txt <> " does not satisfy length <= " <> show len

-- | Predicate for minimum length.
--
-- >>> validate @(MinLength 5) Proxy [1..10]
-- Nothing
--
-- >>> validate @(MinLength 5) Proxy [1..4]
-- Just (MkRefineException {predRep = MinLength 5, targetRep = [Integer], msg = "[1,2,3,4] does not satisfy length >= 5"})
--
-- @since 0.1.0.0
type MinLength :: Nat -> Type
data MinLength n

-- | @since 0.1.0.0
instance
  forall n f a.
  (Foldable f, KnownNat n, Typeable f, Show (f a), Typeable a) =>
  Predicate (MinLength n) (f a)
  where
  validate _ xs
    | length xs >= len = Nothing
    | otherwise = Just $ PC.mkRefineException @(MinLength n) @(f a) err
    where
      len = fromIntegral $ natVal' @n
      err = show xs <> " does not satisfy length >= " <> show len

instance KnownNat n => Predicate (MinLength n) Text where
  validate _ txt
    | T.length txt >= len = Nothing
    | otherwise = Just $ PC.mkRefineException @(MinLength n) @Text err
    where
      len = fromIntegral $ natVal' @n
      err = show txt <> " does not satisfy length >= " <> show len

-- | Predicate for exact length.
--
-- >>> validate @(ExactLength 5) Proxy [1..5]
-- Nothing
--
-- >>> validate @(ExactLength 5) Proxy [1..4]
-- Just (MkRefineException {predRep = ExactLength 5, targetRep = [Integer], msg = "[1,2,3,4] does not satisfy length == 5"})
--
-- @since 0.1.0.0
type ExactLength :: Nat -> Type
data ExactLength n

-- | @since 0.1.0.0
instance
  forall n f a.
  (Foldable f, KnownNat n, Typeable f, Show (f a), Typeable a) =>
  Predicate (ExactLength n) (f a)
  where
  validate _ xs
    | length xs == len = Nothing
    | otherwise = Just $ PC.mkRefineException @(ExactLength n) @(f a) err
    where
      len = fromIntegral $ natVal' @n
      err = show xs <> " does not satisfy length == " <> show len

-- | Predicate for increasing.
--
-- >>> validate @Increasing Proxy [1,1,3,4,5]
-- Nothing
--
-- >>> validate @Increasing Proxy [1,2,5,3]
-- Just (MkRefineException {predRep = Increasing, targetRep = [Integer], msg = "[1,2,5,3] is not increasing."})
--
-- @since 0.1.0.0
type Increasing :: Type
data Increasing

-- | @since 0.1.0.0
instance
  forall f a.
  (Foldable f, Ord a, Show (f a), Typeable f, Typeable a) =>
  Predicate Increasing (f a)
  where
  validate _ xs
    | inOrder (>=) xs = Nothing
    | otherwise = Just $ PC.mkRefineException @Increasing @(f a) err
    where
      err = show xs <> " is not increasing."

-- | Predicate for strictly increasing.
--
-- >>> validate @StrictlyIncreasing Proxy [1,3,4,10]
-- Nothing
--
-- >>> validate @StrictlyIncreasing Proxy [1,1,2,5]
-- Just (MkRefineException {predRep = StrictlyIncreasing, targetRep = [Integer], msg = "[1,1,2,5] is not strictly increasing."})
--
-- @since 0.1.0.0
type StrictlyIncreasing :: Type
data StrictlyIncreasing

-- | @since 0.1.0.0
instance
  forall f a.
  (Foldable f, Ord a, Show (f a), Typeable f, Typeable a) =>
  Predicate StrictlyIncreasing (f a)
  where
  validate _ xs
    | inOrder (>) xs = Nothing
    | otherwise = Just $ PC.mkRefineException @StrictlyIncreasing @(f a) err
    where
      err = show xs <> " is not strictly increasing."

-- | Predicate for decreasing.
--
-- >>> validate @Decreasing Proxy [5,4,4,3,1]
-- Nothing
--
-- >>> validate @Decreasing Proxy [5,4,4,6,1]
-- Just (MkRefineException {predRep = Decreasing, targetRep = [Integer], msg = "[5,4,4,6,1] is not decreasing."})
--
-- @since 0.1.0.0
type Decreasing :: Type
data Decreasing

-- | @since 0.1.0.0
instance
  forall f a.
  (Foldable f, Ord a, Show (f a), Typeable f, Typeable a) =>
  Predicate Decreasing (f a)
  where
  validate _ xs
    | inOrder (<=) xs = Nothing
    | otherwise = Just $ PC.mkRefineException @Decreasing @(f a) err
    where
      err = show xs <> " is not decreasing."

-- | Predicate for decreasing.
--
-- >>> validate @StrictlyDecreasing Proxy [6,4,3,1]
-- Nothing
--
-- >>> validate @StrictlyDecreasing Proxy [5,4,4,2,1]
-- Just (MkRefineException {predRep = StrictlyDecreasing, targetRep = [Integer], msg = "[5,4,4,2,1] is not strictly decreasing."})
--
-- @since 0.1.0.0
type StrictlyDecreasing :: Type
data StrictlyDecreasing

-- | @since 0.1.0.0
instance
  forall f a.
  (Foldable f, Ord a, Show (f a), Typeable f, Typeable a) =>
  Predicate StrictlyDecreasing (f a)
  where
  validate _ xs
    | inOrder (<) xs = Nothing
    | otherwise = Just $ PC.mkRefineException @StrictlyDecreasing @(f a) err
    where
      err = show xs <> " is not strictly decreasing."

data InOrder a
  = Nil
  | Ordered a
  | Fail
  deriving (Eq, Show)

inOrder :: forall f a. Foldable f => (a -> a -> Bool) -> f a -> Bool
inOrder compFn xs = case F.foldl' f Nil xs of
  Fail -> False
  _ -> True
  where
    f Nil x = Ordered x
    f Fail _ = Fail
    f (Ordered y) x =
      if x `compFn` y
        then Ordered x
        else Fail

natVal' :: forall n. KnownNat n => Integer
natVal' = toInteger $ TN.natVal (Proxy @n)
