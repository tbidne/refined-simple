-- | Provides predicates for 'Text' and 'String'.
--
-- @since 0.1.0.0
module Refined.Predicate.Text
  ( Alpha,
    Numeric,
    AlphaNumeric,
    Lower,
    Upper,
    Hex,
  )
where

import Data.Char qualified as C
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as T
import Refined.Predicate.Class (Predicate (..))
import Refined.Predicate.Class qualified as PC

-- $setup
-- >>> import Data.Proxy (Proxy (..))

-- | Predicate for text with alpha characters only.
--
-- >>> satisfies @Alpha Proxy "cat"
-- Nothing
--
-- >>> satisfies @Alpha Proxy "cat5"
-- Just (MkRefineException {predRep = Alpha, targetRep = [Char], msg = "\"cat5\" is not alphabetic characters"})
--
-- @since 0.1.0.0
type Alpha :: Type
data Alpha

-- | @since 0.1.0.0
instance Predicate Alpha Text where
  satisfies _ txt
    | T.all C.isAlpha txt = Nothing
    | otherwise = Just $ PC.mkRefineException @Alpha @Text err
    where
      err = show txt <> " is not alphabetic characters"

-- | @since 0.1.0.0
instance Predicate Alpha String where
  satisfies _ txt
    | all C.isAlpha txt = Nothing
    | otherwise = Just $ PC.mkRefineException @Alpha @String err
    where
      err = show txt <> " is not alphabetic characters"

-- | Predicate for text with numeric characters only.
--
-- >>> satisfies @Numeric Proxy "123"
-- Nothing
--
-- >>> satisfies @Numeric Proxy "123abc"
-- Just (MkRefineException {predRep = Alpha, targetRep = [Char], msg = "\"123abc\" is not numeric characters"})
--
-- @since 0.1.0.0
type Numeric :: Type
data Numeric

-- | @since 0.1.0.0
instance Predicate Numeric Text where
  satisfies _ txt
    | T.all C.isNumber txt = Nothing
    | otherwise = Just $ PC.mkRefineException @Alpha @Text err
    where
      err = show txt <> " is not numeric characters"

-- | @since 0.1.0.0
instance Predicate Numeric String where
  satisfies _ txt
    | all C.isNumber txt = Nothing
    | otherwise = Just $ PC.mkRefineException @Alpha @String err
    where
      err = show txt <> " is not numeric characters"

-- | Predicate for text with alpha or numeric characters only.
--
-- >>> satisfies @AlphaNumeric Proxy "abc123"
-- Nothing
--
-- >>> satisfies @AlphaNumeric Proxy "abc123!"
-- Just (MkRefineException {predRep = Alpha, targetRep = [Char], msg = "\"abc123!\" is not alpha-numeric characters"})
--
-- @since 0.1.0.0
type AlphaNumeric :: Type
data AlphaNumeric

-- | @since 0.1.0.0
instance Predicate AlphaNumeric Text where
  satisfies _ txt
    | T.all C.isAlphaNum txt = Nothing
    | otherwise = Just $ PC.mkRefineException @Alpha @Text err
    where
      err = show txt <> " is not alpha-numeric characters"

-- | @since 0.1.0.0
instance Predicate AlphaNumeric String where
  satisfies _ txt
    | all C.isAlphaNum txt = Nothing
    | otherwise = Just $ PC.mkRefineException @Alpha @String err
    where
      err = show txt <> " is not alpha-numeric characters"

-- | Predicate for lower-case text only.
--
-- >>> satisfies @Lower Proxy "cat"
-- Nothing
--
-- >>> satisfies @Lower Proxy "CAT"
-- Just (MkRefineException {predRep = Alpha, targetRep = [Char], msg = "\"CAT\" is not lowercase"})
--
-- @since 0.1.0.0
type Lower :: Type
data Lower

-- | @since 0.1.0.0
instance Predicate Lower Text where
  satisfies _ txt
    | T.all C.isLower txt = Nothing
    | otherwise = Just $ PC.mkRefineException @Alpha @Text err
    where
      err = show txt <> " is not lowercase"

-- | @since 0.1.0.0
instance Predicate Lower String where
  satisfies _ txt
    | all C.isLower txt = Nothing
    | otherwise = Just $ PC.mkRefineException @Alpha @String err
    where
      err = show txt <> " is not lowercase"

-- | Predicate for lower-case text only.
--
-- >>> satisfies @Upper Proxy "CAT"
-- Nothing
--
-- >>> satisfies @Upper Proxy "cat"
-- Just (MkRefineException {predRep = Alpha, targetRep = [Char], msg = "\"cat\" is not uppercase"})
--
-- @since 0.1.0.0
type Upper :: Type
data Upper

-- | @since 0.1.0.0
instance Predicate Upper Text where
  satisfies _ txt
    | T.all C.isUpper txt = Nothing
    | otherwise = Just $ PC.mkRefineException @Alpha @Text err
    where
      err = show txt <> " is not uppercase"

-- | @since 0.1.0.0
instance Predicate Upper String where
  satisfies _ txt
    | all C.isUpper txt = Nothing
    | otherwise = Just $ PC.mkRefineException @Alpha @String err
    where
      err = show txt <> " is not uppercase"

-- | Predicate for hexadecimal text only.
--
-- >>> satisfies @Hex Proxy "ad381f5c"
-- Nothing
--
-- >>> satisfies @Hex Proxy "ad381f5cxe"
-- Just (MkRefineException {predRep = Hex, targetRep = [Char], msg = "\"ad381f5cxe\" is not hex"})
--
-- @since 0.1.0.0
type Hex :: Type
data Hex

-- | @since 0.1.0.0
instance Predicate Hex Text where
  satisfies _ txt
    | T.all C.isHexDigit txt = Nothing
    | otherwise = Just $ PC.mkRefineException @Hex @Text err
    where
      err = show txt <> " is not hex"

-- | @since 0.1.0.0
instance Predicate Hex String where
  satisfies _ txt
    | all C.isHexDigit txt = Nothing
    | otherwise = Just $ PC.mkRefineException @Hex @String err
    where
      err = show txt <> " is not hex"
