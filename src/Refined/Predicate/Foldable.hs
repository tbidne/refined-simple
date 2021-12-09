{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Provides predicates for 'Foldable'.
--
-- @since 0.1.0.0
module Refined.Predicate.Foldable
  ( -- * Length
    NonEmpty,
    MaxLength,
    MinLength,
    ExactLength,

    -- * Order
    Increasing,
    StrictlyIncreasing,
    Decreasing,
    StrictlyDecreasing,

    -- * Elements
    All,
    Any,
    None,
  )
where

import Control.Applicative ((<|>))
import Data.Foldable qualified as F
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (Proxy (..), Typeable)
import GHC.TypeNats (KnownNat, Nat)
import GHC.TypeNats qualified as TN
import Refined.Predicate.Class (Predicate (..))
import Refined.Predicate.Class qualified as PC
import Refined.Predicate.Operators (Not)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Refined.Predicate.Math

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

-- | Predicate for all elements satisfying some predicate.
--
-- ==== __Examples__
-- >>> satisfies @(All NonZero) Proxy [1..5]
-- Nothing
--
-- >>> satisfies @(All NonZero) Proxy [0..5]
-- Just (MkRefineException {predRep = Not (NatEquals 0), targetRep = Integer, msg = "0 does not satisfy (Not (NatEquals 0))"})
--
-- @since 0.1.0.0
type All :: Type -> Type
data All p

-- | @since 0.1.0.0
instance (Foldable f, Predicate p a) => Predicate (All p) (f a) where
  satisfies _ xs = allSatisfies (satisfies @p Proxy) xs

-- | @since 0.1.0.0
instance Predicate p Char => Predicate (All p) Text where
  satisfies _ txt = allSatisfies (satisfies @p Proxy) str
    where
      str = T.unpack txt

-- | Predicate for any element satisfying some predicate.
--
-- ==== __Examples__
-- >>> satisfies @(Any NonZero) Proxy [0,0,0,4]
-- Nothing
--
-- >>> satisfies @(Any NonZero) Proxy [0,0,0]
-- Just (MkRefineException {predRep = Not (NatEquals 0), targetRep = Integer, msg = "No element satisfied predicate"})
--
-- @since 0.1.0.0
type Any :: Type -> Type
data Any p

-- | @since 0.1.0.0
instance (Foldable f, Predicate p a, Typeable p, Typeable a) => Predicate (Any p) (f a) where
  satisfies _ xs = anySatisfies err (satisfies @p Proxy) xs
    where
      err = PC.mkRefineException @p @a "No element satisfied predicate"

-- | @since 0.1.0.0
instance (Predicate p Char, Typeable p) => Predicate (Any p) Text where
  satisfies _ txt = anySatisfies err (satisfies @p Proxy) str
    where
      str = T.unpack txt
      err = PC.mkRefineException @p @Char "No element satisfied predicate"

-- | Predicate for no elements satisfying a predicate.
--
-- ==== __Examples__
-- >>> satisfies @(None (NatEquals 2)) Proxy [3,4,5]
-- Nothing
--
-- >>> satisfies @(None (NatEquals 2)) Proxy [3,4,2,5]
-- Just (MkRefineException {predRep = Not (Any (NatEquals 2)), targetRep = [Integer], msg = "[3,4,2,5] does not satisfy (Not (Any (NatEquals 2)))"})
--
-- @since 0.1.0.0
type None :: Type -> Type

type None p = Not (Any p)

allSatisfies :: Foldable f => (a -> Maybe b) -> f a -> Maybe b
allSatisfies p = foldr (\x acc -> p x <|> acc) Nothing

anySatisfies :: Foldable f => b -> (a -> Maybe b) -> f a -> Maybe b
anySatisfies defErr p = foldr f (Just defErr)
  where
    f x acc = case p x of
      Just _ -> acc
      Nothing -> Nothing

natVal' :: forall n. KnownNat n => Integer
natVal' = toInteger $ TN.natVal (Proxy @n)
