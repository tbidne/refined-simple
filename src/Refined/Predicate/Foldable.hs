{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Provides predicates for 'Foldable'.
module Refined.Predicate.Foldable
  ( MaxLength,
    SortedAsc,
    SortedDesc,
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

-- | Predicate for ascended sorted.
--
-- >>> validate @SortedAsc Proxy [1,1,3,4,5]
-- Nothing
--
-- >>> validate @SortedAsc Proxy [1,2,5,3]
-- Just (MkRefineException {predRep = SortedAsc, targetRep = [Integer], msg = "[1,2,5,3] is not in ascending order."})
--
-- @since 0.1.0.0
--
-- @since 0.1.0.0
type SortedAsc :: Type
data SortedAsc

-- | @since 0.1.0.0
instance
  forall f a.
  (Foldable f, Ord a, Show (f a), Typeable f, Typeable a) =>
  Predicate SortedAsc (f a)
  where
  validate _ xs
    | isSortedAsc xs = Nothing
    | otherwise = Just $ PC.mkRefineException @SortedAsc @(f a) err
    where
      err = show xs <> " is not in ascending order."

data IsSortedAsc a
  = NilAsc
  | SortedAsc a
  | FailAsc
  deriving (Eq, Show)

isSortedAsc :: forall f a. (Foldable f, Ord a) => f a -> Bool
isSortedAsc xs = case F.foldl' f NilAsc xs of
  FailAsc -> False
  _ -> True
  where
    f NilAsc x = SortedAsc x
    f FailAsc _ = FailAsc
    f (SortedAsc y) x =
      if x >= y
        then SortedAsc x
        else FailAsc

-- | Predicate for descended sorted.
--
-- >>> validate @SortedDesc Proxy [5,4,4,3,1]
-- Nothing
--
-- >>> validate @SortedDesc Proxy [5,4,4,6,1]
-- Just (MkRefineException {predRep = SortedAsc, targetRep = [Integer], msg = "[5,4,4,6,1] is not in descending order."})
--
-- @since 0.1.0.0
type SortedDesc :: Type
data SortedDesc

-- | @since 0.1.0.0
instance
  forall f a.
  (Foldable f, Ord a, Show (f a), Typeable f, Typeable a) =>
  Predicate SortedDesc (f a)
  where
  validate _ xs
    | isSortedDesc xs = Nothing
    | otherwise = Just $ PC.mkRefineException @SortedAsc @(f a) err
    where
      err = show xs <> " is not in descending order."

data IsSortedDesc a
  = NilDesc
  | SortedDesc a
  | FailDesc
  deriving (Eq, Show)

isSortedDesc :: forall f a. (Foldable f, Ord a) => f a -> Bool
isSortedDesc xs = case F.foldl' f NilDesc xs of
  FailDesc -> False
  _ -> True
  where
    f NilDesc x = SortedDesc x
    f FailDesc _ = FailDesc
    f (SortedDesc y) x =
      if x <= y
        then SortedDesc x
        else FailDesc

natVal' :: forall n. KnownNat n => Integer
natVal' = toInteger $ TN.natVal (Proxy @n)
