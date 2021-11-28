{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Provides predicates for 'Foldable'.
--
-- @since 0.1.0.0
module Refined.Predicate.Foldable
  ( NonEmpty,
    MaxLength,
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

-- $setup
-- >>> :set -XOverloadedStrings

-- | Predicate for non-empty.
--
-- ==== __Examples__
-- >>> satisfies @NonEmpty Proxy [1]
-- Nothing
--
-- >>> satisfies @NonEmpty @Text Proxy ""
-- Just (MkRefineException {predRep = NonEmpty, targetRep = Text, msg = "\"\" is empty"})
--
-- @since 0.1.0.0
type NonEmpty :: Type
data NonEmpty

-- | @since 0.1.0.0
instance
  forall f a.
  (Foldable f, Typeable f, Show (f a), Typeable a) =>
  Predicate NonEmpty (f a)
  where
  satisfies _ xs
    | (not . null) xs = Nothing
    | otherwise = Just $ PC.mkRefineException @NonEmpty @(f a) err
    where
      err = show xs <> " is empty"

-- | @since 0.1.0.0
instance Predicate NonEmpty Text where
  satisfies _ txt
    | (not . T.null) txt = Nothing
    | otherwise = Just $ PC.mkRefineException @NonEmpty @Text err
    where
      err = show txt <> " is empty"

-- | Predicate for maximum length.
--
-- ==== __Examples__
-- >>> satisfies @(MaxLength 4) Proxy [1..4]
-- Nothing
--
-- >>> satisfies @(MaxLength 4) Proxy [1..5]
-- Just (MkRefineException {predRep = MaxLength 4, targetRep = [Integer], msg = "[1,2,3,4,5] does not satisfy length <= 4"})
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
  satisfies _ xs
    | length xs <= len = Nothing
    | otherwise = Just $ PC.mkRefineException @(MaxLength n) @(f a) err
    where
      len = fromIntegral $ natVal' @n
      err = show xs <> " does not satisfy length <= " <> show len

-- | @since 0.1.0.0
instance KnownNat n => Predicate (MaxLength n) Text where
  satisfies _ txt
    | T.length txt <= len = Nothing
    | otherwise = Just $ PC.mkRefineException @(MaxLength n) @Text err
    where
      len = fromIntegral $ natVal' @n
      err = show txt <> " does not satisfy length <= " <> show len

-- | Predicate for minimum length.
--
-- ==== __Examples__
-- >>> satisfies @(MinLength 5) Proxy [1..10]
-- Nothing
--
-- >>> satisfies @(MinLength 5) Proxy [1..4]
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
  satisfies _ xs
    | length xs >= len = Nothing
    | otherwise = Just $ PC.mkRefineException @(MinLength n) @(f a) err
    where
      len = fromIntegral $ natVal' @n
      err = show xs <> " does not satisfy length >= " <> show len

-- | @since 0.1.0.0
instance KnownNat n => Predicate (MinLength n) Text where
  satisfies _ txt
    | T.length txt >= len = Nothing
    | otherwise = Just $ PC.mkRefineException @(MinLength n) @Text err
    where
      len = fromIntegral $ natVal' @n
      err = show txt <> " does not satisfy length >= " <> show len

-- | Predicate for exact length.
--
-- ==== __Examples__
-- >>> satisfies @(ExactLength 5) Proxy [1..5]
-- Nothing
--
-- >>> satisfies @(ExactLength 5) Proxy [1..4]
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
  satisfies _ xs
    | length xs == len = Nothing
    | otherwise = Just $ PC.mkRefineException @(ExactLength n) @(f a) err
    where
      len = fromIntegral $ natVal' @n
      err = show xs <> " does not satisfy length == " <> show len

-- | @since 0.1.0.0
instance forall n. (KnownNat n) => Predicate (ExactLength n) Text where
  satisfies _ txt
    | T.length txt == len = Nothing
    | otherwise = Just $ PC.mkRefineException @(ExactLength n) @Text err
    where
      len = fromIntegral $ natVal' @n
      err = show txt <> " does not satisfy length == " <> show len

-- | Predicate for increasing.
--
-- ==== __Examples__
-- >>> satisfies @Increasing Proxy [1,1,3,4,5]
-- Nothing
--
-- >>> satisfies @Increasing Proxy [1,2,5,3]
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
  satisfies _ xs
    | inOrder (>=) xs = Nothing
    | otherwise = Just $ PC.mkRefineException @Increasing @(f a) err
    where
      err = show xs <> " is not increasing."

-- | @since 0.1.0.0
instance Predicate Increasing Text where
  satisfies _ txt
    | inOrder (>=) str = Nothing
    | otherwise = Just $ PC.mkRefineException @Increasing @Text err
    where
      str = T.unpack txt
      err = show str <> " is not increasing."

-- | Predicate for strictly increasing.
--
-- ==== __Examples__
-- >>> satisfies @StrictlyIncreasing Proxy [1,3,4,10]
-- Nothing
--
-- >>> satisfies @StrictlyIncreasing Proxy [1,1,2,5]
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
  satisfies _ xs
    | inOrder (>) xs = Nothing
    | otherwise = Just $ PC.mkRefineException @StrictlyIncreasing @(f a) err
    where
      err = show xs <> " is not strictly increasing."

-- | @since 0.1.0.0
instance Predicate StrictlyIncreasing Text where
  satisfies _ txt
    | inOrder (>) str = Nothing
    | otherwise = Just $ PC.mkRefineException @StrictlyIncreasing @Text err
    where
      str = T.unpack txt
      err = show str <> " is not strictly increasing."

-- | Predicate for decreasing.
--
-- ==== __Examples__
-- >>> satisfies @Decreasing Proxy [5,4,4,3,1]
-- Nothing
--
-- >>> satisfies @Decreasing Proxy [5,4,4,6,1]
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
  satisfies _ xs
    | inOrder (<=) xs = Nothing
    | otherwise = Just $ PC.mkRefineException @Decreasing @(f a) err
    where
      err = show xs <> " is not decreasing."

-- | @since 0.1.0.0
instance Predicate Decreasing Text where
  satisfies _ txt
    | inOrder (<=) str = Nothing
    | otherwise = Just $ PC.mkRefineException @Decreasing @Text err
    where
      str = T.unpack txt
      err = show str <> " is not decreasing."

-- | Predicate for strictly decreasing.
--
-- ==== __Examples__
-- >>> satisfies @StrictlyDecreasing Proxy [6,4,3,1]
-- Nothing
--
-- >>> satisfies @StrictlyDecreasing Proxy [5,4,4,2,1]
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
  satisfies _ xs
    | inOrder (<) xs = Nothing
    | otherwise = Just $ PC.mkRefineException @StrictlyDecreasing @(f a) err
    where
      err = show xs <> " is not strictly decreasing."

-- | @since 0.1.0.0
instance Predicate StrictlyDecreasing Text where
  satisfies _ txt
    | inOrder (<) str = Nothing
    | otherwise = Just $ PC.mkRefineException @StrictlyDecreasing @Text err
    where
      str = T.unpack txt
      err = show str <> " is not strictly decreasing."

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
