-- | Provides predicates for 'Text' and 'String'.
--
-- @since 0.1.0.0
module Refined.Predicate.Text
  ( -- * Symbol Equality
    SymEquals,
    SymNotEquals,

    -- * Char predicates
    -- $char
    Alpha,
    Numeric,
    AlphaNum,
    Lower,
    Upper,
    Hex,
  )
where

import Data.Char qualified as C
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.TypeLits (KnownSymbol, Symbol)
import GHC.TypeLits qualified as TL
import Refined.Predicate.Class (Predicate (..))
import Refined.Predicate.Class qualified as PC
import Refined.Predicate.Operators (Not)

-- $setup
-- >>> import Data.Proxy (Proxy (..))
-- >>> import Refined.Predicate.Foldable (All)
-- >>> :set -XOverloadedStrings

-- | Predicate equality for symbols.
--
-- ==== __Examples__
-- >>> satisfies @(SymEquals "c") Proxy 'c'
-- Nothing
--
-- >>> satisfies @(SymEquals "abc") Proxy 'c'
-- Just (MkRefineException {predRep = SymEquals "abc", targetRep = Char, msg = "c is not a single Char"})
--
-- >>> satisfies @(SymEquals "abc") Proxy "abc"
-- Nothing
--
-- >>> satisfies @(SymEquals "123") @Text Proxy "abc"
-- Just (MkRefineException {predRep = SymEquals "123", targetRep = Text, msg = "abc does not equal the predicate"})
--
-- @since 0.1.0.0
type SymEquals :: Symbol -> Type
data SymEquals c

-- | The logical negation of 'SymEquals'.
--
-- ==== __Examples__
-- >>> satisfies @(SymNotEquals "c") Proxy 'x'
-- Nothing
--
-- >>> satisfies @(SymNotEquals "abc") Proxy "abc"
-- Just (MkRefineException {predRep = Not (SymEquals "abc"), targetRep = [Char], msg = "\"abc\" does not satisfy (Not (SymEquals \"abc\"))"})
--
-- @since 0.1.0.0
type SymNotEquals :: Symbol -> Type

type SymNotEquals c = Not (SymEquals c)

-- | @since 0.1.0.0
instance KnownSymbol c => Predicate (SymEquals c) Char where
  satisfies _ x = case sym of
    [y] ->
      if x == y
        then Nothing
        else Just $ PC.mkRefineException @(SymEquals c) @Char eqErr
    _ -> Just $ PC.mkRefineException @(SymEquals c) @Char nonCharErr
    where
      sym = TL.symbolVal @c Proxy
      eqErr = x : " does not equal the predicate"
      nonCharErr = x : " is not a single Char"

-- | @since 0.1.0.0
instance KnownSymbol c => Predicate (SymEquals c) String where
  satisfies _ txt
    | txt == sym = Nothing
    | otherwise = Just $ PC.mkRefineException @(SymEquals c) @Text err
    where
      sym = TL.symbolVal @c Proxy
      err = txt <> " does not equal the predicate"

-- | @since 0.1.0.0
instance KnownSymbol c => Predicate (SymEquals c) Text where
  satisfies _ txt
    | txt == sym = Nothing
    | otherwise = Just $ PC.mkRefineException @(SymEquals c) @Text err
    where
      sym = T.pack $ TL.symbolVal @c Proxy
      err = T.unpack txt <> " does not equal the predicate"

-- $char
-- These predicates work on 'Char', though they can be extended to 'String'
-- and 'Text' via foldable's 'Refined.Predicate.Foldable.All'.
--
-- ==== __Examples__
-- >>> satisfies @(All Alpha) Proxy "abc"
-- Nothing
--
-- >>> satisfies @(All Alpha) Proxy "abc1"
-- Just (MkRefineException {predRep = Alpha, targetRep = Char, msg = "1 is not an alphabetic character"})

-- | Predicate for an alpha character.
--
-- >>> satisfies @Alpha Proxy 'c'
-- Nothing
--
-- >>> satisfies @Alpha Proxy '7'
-- Just (MkRefineException {predRep = Alpha, targetRep = Char, msg = "7 is not an alphabetic character"})
--
-- @since 0.1.0.0
type Alpha :: Type
data Alpha

-- | @since 0.1.0.0
instance Predicate Alpha Char where
  satisfies _ c
    | C.isAlpha c = Nothing
    | otherwise = Just $ PC.mkRefineException @Alpha @Char err
    where
      err = c : " is not an alphabetic character"

-- | Predicate for a numeric character.
--
-- >>> satisfies @Numeric Proxy '1'
-- Nothing
--
-- >>> satisfies @Numeric Proxy 'a'
-- Just (MkRefineException {predRep = Numeric, targetRep = Char, msg = "a is not a numeric character"})
--
-- @since 0.1.0.0
type Numeric :: Type
data Numeric

-- | @since 0.1.0.0
instance Predicate Numeric Char where
  satisfies _ c
    | C.isNumber c = Nothing
    | otherwise = Just $ PC.mkRefineException @Numeric @Char err
    where
      err = c : " is not a numeric character"

-- | Predicate for an alpha or numeric character.
--
-- >>> satisfies @AlphaNum Proxy 'a'
-- Nothing
--
-- >>> satisfies @AlphaNum Proxy '1'
-- Nothing
--
-- >>> satisfies @AlphaNum Proxy '!'
-- Just (MkRefineException {predRep = AlphaNum, targetRep = Char, msg = "! is not alpha-numeric characters"})
--
-- @since 0.1.0.0
type AlphaNum :: Type
data AlphaNum

-- | @since 0.1.0.0
instance Predicate AlphaNum Char where
  satisfies _ c
    | C.isAlphaNum c = Nothing
    | otherwise = Just $ PC.mkRefineException @AlphaNum @Char err
    where
      err = c : " is not alpha-numeric characters"

-- | Predicate for a lower-case character.
--
-- >>> satisfies @Lower Proxy 'c'
-- Nothing
--
-- >>> satisfies @Lower Proxy 'C'
-- Just (MkRefineException {predRep = Lower, targetRep = Char, msg = "C is not lowercase"})
--
-- @since 0.1.0.0
type Lower :: Type
data Lower

-- | @since 0.1.0.0
instance Predicate Lower Char where
  satisfies _ c
    | C.isLower c = Nothing
    | otherwise = Just $ PC.mkRefineException @Lower @Char err
    where
      err = c : " is not lowercase"

-- | Predicate for ab upper-case character.
--
-- >>> satisfies @Upper Proxy 'C'
-- Nothing
--
-- >>> satisfies @Upper Proxy 'c'
-- Just (MkRefineException {predRep = Upper, targetRep = Char, msg = "c is not uppercase"})
--
-- @since 0.1.0.0
type Upper :: Type
data Upper

-- | @since 0.1.0.0
instance Predicate Upper Char where
  satisfies _ c
    | C.isUpper c = Nothing
    | otherwise = Just $ PC.mkRefineException @Upper @Char err
    where
      err = c : " is not uppercase"

-- | Predicate for a hexadecimal character.
--
-- >>> satisfies @Hex Proxy '1'
-- Nothing
--
-- >>> satisfies @Hex Proxy 'f'
-- Nothing
--
-- >>> satisfies @Hex Proxy 'g'
-- Just (MkRefineException {predRep = Hex, targetRep = Char, msg = "g is not hex"})
--
-- @since 0.1.0.0
type Hex :: Type
data Hex

-- | @since 0.1.0.0
instance Predicate Hex Char where
  satisfies _ c
    | C.isHexDigit c = Nothing
    | otherwise = Just $ PC.mkRefineException @Hex @Char err
    where
      err = c : " is not hex"
