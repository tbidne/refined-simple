-- | Provides predicates for 'Text' and 'String'.
module Refined.Predicate.Text
  ( Alpha,
    Numeric,
    AlphaNumeric,
    Lower,
    Upper,
  )
where

import Data.Char qualified as C
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as T
import Refined.Predicate.Class (Predicate (..))
import Refined.Predicate.Class qualified as PC

-- | Predicate for text with alpha characters only.
--
-- @since 0.1.0.0
type Alpha :: Type
data Alpha

-- | @since 0.1.0.0
instance Predicate Alpha Text where
  validate _ txt
    | T.all C.isAlpha txt = Nothing
    | otherwise = Just $ PC.mkRefineException @Alpha @Text err
    where
      err = show txt <> " is not alphabetic characters"

-- | @since 0.1.0.0
instance Predicate Alpha String where
  validate _ txt
    | all C.isAlpha txt = Nothing
    | otherwise = Just $ PC.mkRefineException @Alpha @String err
    where
      err = show txt <> " is not alphabetic characters"

-- | Predicate for text with numeric characters only.
--
-- @since 0.1.0.0
type Numeric :: Type
data Numeric

-- | @since 0.1.0.0
instance Predicate Numeric Text where
  validate _ txt
    | T.all C.isNumber txt = Nothing
    | otherwise = Just $ PC.mkRefineException @Alpha @Text err
    where
      err = show txt <> " is not alphabetic characters"

-- | @since 0.1.0.0
instance Predicate Numeric String where
  validate _ txt
    | all C.isNumber txt = Nothing
    | otherwise = Just $ PC.mkRefineException @Alpha @String err
    where
      err = show txt <> " is not alphabetic characters"

-- | Predicate for text with alpha or numeric characters only.
--
-- @since 0.1.0.0
type AlphaNumeric :: Type
data AlphaNumeric

-- | @since 0.1.0.0
instance Predicate AlphaNumeric Text where
  validate _ txt
    | T.all C.isAlphaNum txt = Nothing
    | otherwise = Just $ PC.mkRefineException @Alpha @Text err
    where
      err = show txt <> " is not alpha-numeric characters"

-- | @since 0.1.0.0
instance Predicate AlphaNumeric String where
  validate _ txt
    | all C.isAlphaNum txt = Nothing
    | otherwise = Just $ PC.mkRefineException @Alpha @String err
    where
      err = show txt <> " is not alpha-numeric characters"

-- | Predicate for lower-case text only.
--
-- @since 0.1.0.0
type Lower :: Type
data Lower

-- | @since 0.1.0.0
instance Predicate Lower Text where
  validate _ txt
    | T.all C.isLower txt = Nothing
    | otherwise = Just $ PC.mkRefineException @Alpha @Text err
    where
      err = show txt <> " is not lowercase"

-- | @since 0.1.0.0
instance Predicate Lower String where
  validate _ txt
    | all C.isLower txt = Nothing
    | otherwise = Just $ PC.mkRefineException @Alpha @String err
    where
      err = show txt <> " is not lowercase"

-- | Predicate for lower-case text only.
--
-- @since 0.1.0.0
type Upper :: Type
data Upper

-- | @since 0.1.0.0
instance Predicate Upper Text where
  validate _ txt
    | T.all C.isUpper txt = Nothing
    | otherwise = Just $ PC.mkRefineException @Alpha @Text err
    where
      err = show txt <> " is not uppercase"

-- | @since 0.1.0.0
instance Predicate Upper String where
  validate _ txt
    | all C.isUpper txt = Nothing
    | otherwise = Just $ PC.mkRefineException @Alpha @String err
    where
      err = show txt <> " is not uppercase"
