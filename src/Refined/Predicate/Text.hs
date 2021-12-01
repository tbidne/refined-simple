-- | Provides predicates for 'Text' and 'String'.
--
-- @since 0.1.0.0
module Refined.Predicate.Text
  ( -- * Symbol Equality
    SymEquals,
    SymNotEquals,

    -- * Char predicates
    -- $char

    -- ** Character Classification
    Control,
    Space,
    Lower,
    Upper,
    Alpha,
    AlphaNum,
    Digit,
    OctDigit,
    HexDigit,
    Letter,
    Mark,
    Number,
    Punctuation,
    Symbol,
    Separator,

    -- *** Subranges
    Ascii,
    Latin1,
    AsciiUpper,
    AsciiLower,
  )
where

import Data.Char qualified as C
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.TypeLits (KnownSymbol)
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
type SymEquals :: TL.Symbol -> Type
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
type SymNotEquals :: TL.Symbol -> Type

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
-- These section models the boolean functions defined in "Data.Char". See
-- that module for more information regarding these definitions.
--
-- Although the instances are defined for 'Char', they can be
-- extended to 'String' and 'Text' via "Refined.Predicate.Foldable"'s
-- 'Refined.Predicate.Foldable.All'.
--
-- ==== __Examples__
-- >>> satisfies @(All Alpha) Proxy "abc"
-- Nothing
--
-- >>> satisfies @(All Alpha) Proxy "abc1"
-- Just (MkRefineException {predRep = Alpha, targetRep = Char, msg = "1 is not an alphabetic character"})

-- | Predicate for 'C.isControl'.
--
-- ==== __Examples__
-- >>> satisfies @Control Proxy '\r'
-- Nothing
--
-- >>> satisfies @Control Proxy 'a'
-- Just (MkRefineException {predRep = Control, targetRep = Char, msg = "a is not a control character"})
--
-- @since 0.1.0.0
type Control :: Type
data Control

-- | @since 0.1.0.0
instance Predicate Control Char where
  satisfies _ c
    | C.isControl c = Nothing
    | otherwise = Just $ PC.mkRefineException @Control @Char err
    where
      err = c : " is not a control character"

-- | Predicate for a 'C.isSpace'.
--
-- ==== __Examples__
-- >>> satisfies @Space Proxy ' '
-- Nothing
-- >>> satisfies @Space Proxy '\r'
-- Nothing
--
-- >>> satisfies @Space Proxy 'a'
-- Just (MkRefineException {predRep = Space, targetRep = Char, msg = "a is not a space character"})
--
-- @since 0.1.0.0
type Space :: Type
data Space

-- | @since 0.1.0.0
instance Predicate Space Char where
  satisfies _ c
    | C.isSpace c = Nothing
    | otherwise = Just $ PC.mkRefineException @Space @Char err
    where
      err = c : " is not a space character"

-- | Predicate for 'C.isLower'.
--
-- ==== __Examples__
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

-- | Predicate for 'C.isUpper'.
--
-- ==== __Examples__
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

-- | Predicate for 'C.isAlpha'.
--
-- ==== __Examples__
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

-- | Predicate for 'C.isAlphaNum'.
--
-- ==== __Examples__
-- >>> satisfies @AlphaNum Proxy 'a'
-- Nothing
--
-- >>> satisfies @AlphaNum Proxy '1'
-- Nothing
--
-- >>> satisfies @AlphaNum Proxy '!'
-- Just (MkRefineException {predRep = AlphaNum, targetRep = Char, msg = "! is not an alpha-numeric character"})
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
      err = c : " is not an alpha-numeric character"

-- | Predicate for 'C.isPrint'.
--
-- ==== __Examples__
-- >>> satisfies @Print Proxy 'a'
-- Nothing
--
-- >>> satisfies @Print Proxy '\v'
-- Just (MkRefineException {predRep = AlphaNum, targetRep = Char, msg = "\v is not a printable character"})
--
-- @since 0.1.0.0
type Print :: Type
data Print

-- | @since 0.1.0.0
instance Predicate Print Char where
  satisfies _ c
    | C.isPrint c = Nothing
    | otherwise = Just $ PC.mkRefineException @AlphaNum @Char err
    where
      err = c : " is not a printable character"

-- | Predicate for 'C.isDigit'.
--
-- ==== __Examples__
-- >>> satisfies @Digit Proxy '1'
-- Nothing
--
-- >>> satisfies @Digit Proxy 'a'
-- Just (MkRefineException {predRep = Digit, targetRep = Char, msg = "a is not a digit"})
--
-- @since 0.1.0.0
type Digit :: Type
data Digit

-- | @since 0.1.0.0
instance Predicate Digit Char where
  satisfies _ c
    | C.isDigit c = Nothing
    | otherwise = Just $ PC.mkRefineException @Digit @Char err
    where
      err = c : " is not a digit"

-- | Predicate for 'C.isOctDigit'.
--
-- ==== __Examples__
-- >>> satisfies @OctDigit Proxy '4'
-- Nothing
--
-- >>> satisfies @OctDigit Proxy '9'
-- Just (MkRefineException {predRep = OctDigit, targetRep = Char, msg = "9 is not an octal digit"})
--
-- @since 0.1.0.0
type OctDigit :: Type
data OctDigit

-- | @since 0.1.0.0
instance Predicate OctDigit Char where
  satisfies _ c
    | C.isOctDigit c = Nothing
    | otherwise = Just $ PC.mkRefineException @OctDigit @Char err
    where
      err = c : " is not an octal digit"

-- | Predicate for 'C.isHexDigit'.
--
-- >>> satisfies @HexDigit Proxy '1'
-- Nothing
--
-- ==== __Examples__
-- >>> satisfies @HexDigit Proxy 'f'
-- Nothing
--
-- >>> satisfies @HexDigit Proxy 'g'
-- Just (MkRefineException {predRep = HexDigit, targetRep = Char, msg = "g is not a hexadecimal digit"})
--
-- @since 0.1.0.0
type HexDigit :: Type
data HexDigit

-- | @since 0.1.0.0
instance Predicate HexDigit Char where
  satisfies _ c
    | C.isHexDigit c = Nothing
    | otherwise = Just $ PC.mkRefineException @HexDigit @Char err
    where
      err = c : " is not a hexadecimal digit"

-- | Predicate for 'C.isLetter'.
--
-- ==== __Examples__
-- >>> satisfies @Letter Proxy 'f'
-- Nothing
--
-- >>> satisfies @Letter Proxy '\r'
-- Just (MkRefineException {predRep = Letter, targetRep = Char, msg = "\r is not a letter"})
--
-- @since 0.1.0.0
type Letter :: Type
data Letter

-- | @since 0.1.0.0
instance Predicate Letter Char where
  satisfies _ c
    | C.isLetter c = Nothing
    | otherwise = Just $ PC.mkRefineException @Letter @Char err
    where
      err = c : " is not a letter"

-- | Predicate for 'C.isMark'.
--
-- ==== __Examples__
-- >>> satisfies @Mark Proxy '\x20DD'
-- Nothing
--
-- >>> satisfies @Mark Proxy 'a'
-- Just (MkRefineException {predRep = Mark, targetRep = Char, msg = "a is not a mark"})
--
-- @since 0.1.0.0
type Mark :: Type
data Mark

-- | @since 0.1.0.0
instance Predicate Mark Char where
  satisfies _ c
    | C.isMark c = Nothing
    | otherwise = Just $ PC.mkRefineException @Mark @Char err
    where
      err = c : " is not a mark"

-- | Predicate for 'C.isNumber'.
--
-- ==== __Examples__
-- >>> satisfies @Number Proxy '2'
-- Nothing
--
-- >>> satisfies @Number Proxy 'a'
-- Just (MkRefineException {predRep = Number, targetRep = Char, msg = "a is not a number"})
--
-- @since 0.1.0.0
type Number :: Type
data Number

-- | @since 0.1.0.0
instance Predicate Number Char where
  satisfies _ c
    | C.isNumber c = Nothing
    | otherwise = Just $ PC.mkRefineException @Number @Char err
    where
      err = c : " is not a number"

-- | Predicate for 'C.isPunctuation'.
--
-- ==== __Examples__
-- >>> satisfies @Punctuation Proxy '!'
-- Nothing
--
-- >>> satisfies @Punctuation Proxy 'a'
-- Just (MkRefineException {predRep = Punctuation, targetRep = Char, msg = "a is not punctuation"})
--
-- @since 0.1.0.0
type Punctuation :: Type
data Punctuation

-- | @since 0.1.0.0
instance Predicate Punctuation Char where
  satisfies _ c
    | C.isPunctuation c = Nothing
    | otherwise = Just $ PC.mkRefineException @Punctuation @Char err
    where
      err = c : " is not punctuation"

-- | Predicate for 'C.isSymbol'.
--
-- ==== __Examples__
-- >>> satisfies @Symbol Proxy '$'
-- Nothing
--
-- >>> satisfies @Symbol Proxy 'a'
-- Just (MkRefineException {predRep = Symbol, targetRep = Char, msg = "a is not a symbol"})
--
-- @since 0.1.0.0
type Symbol :: Type
data Symbol

-- | @since 0.1.0.0
instance Predicate Symbol Char where
  satisfies _ c
    | C.isSymbol c = Nothing
    | otherwise = Just $ PC.mkRefineException @Symbol @Char err
    where
      err = c : " is not a symbol"

-- | Predicate for 'C.isSeparator'.
--
-- ==== __Examples__
-- >>> satisfies @Separator Proxy ' '
-- Nothing
--
-- >>> satisfies @Separator Proxy 'a'
-- Just (MkRefineException {predRep = Separator, targetRep = Char, msg = "a is not a separator"})
--
-- @since 0.1.0.0
type Separator :: Type
data Separator

-- | @since 0.1.0.0
instance Predicate Separator Char where
  satisfies _ c
    | C.isSeparator c = Nothing
    | otherwise = Just $ PC.mkRefineException @Separator @Char err
    where
      err = c : " is not a separator"

-- | Predicate for 'C.isAscii'.
--
-- ==== __Examples__
-- >>> satisfies @Ascii Proxy 'a'
-- Nothing
--
-- >>> satisfies @Ascii Proxy '\x20DD'
-- Just (MkRefineException {predRep = Ascii, targetRep = Char, msg = "\8413 is not ascii"})
--
-- @since 0.1.0.0
type Ascii :: Type
data Ascii

-- | @since 0.1.0.0
instance Predicate Ascii Char where
  satisfies _ c
    | C.isAscii c = Nothing
    | otherwise = Just $ PC.mkRefineException @Ascii @Char err
    where
      err = c : " is not ascii"

-- | Predicate for 'C.Latin1'.
--
-- ==== __Examples__
-- >>> satisfies @Latin1 Proxy 'a'
-- Nothing
--
-- >>> satisfies @Latin1 Proxy '\x20DD'
-- Just (MkRefineException {predRep = Latin1, targetRep = Char, msg = "\8413 is not latin1"})
--
-- @since 0.1.0.0
type Latin1 :: Type
data Latin1

-- | @since 0.1.0.0
instance Predicate Latin1 Char where
  satisfies _ c
    | C.isLatin1 c = Nothing
    | otherwise = Just $ PC.mkRefineException @Latin1 @Char err
    where
      err = c : " is not latin1"

-- | Predicate for 'C.isAsciiUpper'.
--
-- ==== __Examples__
-- >>> satisfies @AsciiUpper Proxy 'A'
-- Nothing
--
-- >>> satisfies @AsciiUpper Proxy 'a'
-- Just (MkRefineException {predRep = AsciiUpper, targetRep = Char, msg = "a is not uppercase ascii"})
--
-- @since 0.1.0.0
type AsciiUpper :: Type
data AsciiUpper

-- | @since 0.1.0.0
instance Predicate AsciiUpper Char where
  satisfies _ c
    | C.isAsciiUpper c = Nothing
    | otherwise = Just $ PC.mkRefineException @AsciiUpper @Char err
    where
      err = c : " is not uppercase ascii"

-- | Predicate for 'C.isAsciiLower'.
--
-- ==== __Examples__
-- >>> satisfies @AsciiLower Proxy 'a'
-- Nothing
--
-- >>> satisfies @AsciiLower Proxy 'A'
-- Just (MkRefineException {predRep = AsciiLower, targetRep = Char, msg = "A is not lowercase ascii"})
--
-- @since 0.1.0.0
type AsciiLower :: Type
data AsciiLower

-- | @since 0.1.0.0
instance Predicate AsciiLower Char where
  satisfies _ c
    | C.isAsciiLower c = Nothing
    | otherwise = Just $ PC.mkRefineException @AsciiLower @Char err
    where
      err = c : " is not lowercase ascii"
