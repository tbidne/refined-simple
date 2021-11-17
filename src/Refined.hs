{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This is the main entry point to the library. It provides the core
-- types and functions for usage.
module Refined
  ( -- * Types

    -- ** Refined
    Refined (MkRefined, unrefine),
    refineEmpty,
    refine,
    refineTH,
    unsafeRefine,

    -- ** RefineException
    RefineException (..),
    Predicate.mkRefineException,

    -- * Predicates
    Predicate (..),

    -- ** Proving
    prove,
    unsafeProve,
    relax,
    relaxAll,

    -- ** TypeErrors
    Implies,
    PredNotFound,

    -- ** Built-In
    NotEquals,
    GreaterThanEq,
    GreaterThan,
    LessThanEq,
    LessThan,
    Even,
    Odd,

    -- ** Specializations
    NonZero,
    NonNegative,
    Positive,
    NonPositive,
    Negative,

    -- * Misc Utils
    AppendP,
    DeleteP,
    ImpliesBool,
    ErrIfFalse,
  )
where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Language.Haskell.TH.Syntax (Lift, Q, TExp)
import Language.Haskell.TH.Syntax qualified as TH
import Refined.Internal (RefineException (..), Refined (..))
import Refined.Predicate
  ( Even,
    GreaterThan,
    GreaterThanEq,
    LessThan,
    LessThanEq,
    Negative,
    NonNegative,
    NonPositive,
    NonZero,
    NotEquals,
    Odd,
    Positive,
    Predicate (..),
  )
import Refined.Predicate qualified as Predicate

-- $setup
-- >>> safeDiv :: (Implies ps NonZero, Integral n) => n -> Refined ps n -> n; safeDiv x d = x `div` unrefine d

-- | Wraps @a@ in 'Refined' with no attached predicates.
--
-- >>> refineEmpty 0
-- UnsafeRefined {unrefine = 0}
--
-- @since 0.1.0.0
refineEmpty :: a -> Refined '[] a
refineEmpty = UnsafeRefined

-- | Attempts to prove the given predicate. If it succeeds, we return the
-- refined @a@. Otherwise we return an error.
--
-- >>> let x = refine @Positive @Int 5
-- >>> :type x
-- x :: Either RefineException (Refined '[Positive] Int)
--
-- >>> x
-- Right (UnsafeRefined {unrefine = 5})
--
-- >>> refine @Positive 0
-- Left (MkRefineException {predRep = GreaterThan 0, targetRep = Integer, msg = "0 does not satisfy > 0"})
--
-- @since 0.1.0.0
refine :: forall p a. Predicate p a => a -> Either RefineException (Refined '[p] a)
refine x = case validate @p Proxy x of
  Nothing -> Right $ UnsafeRefined x
  Just ex -> Left ex

-- | Proves @p@ at compile-time via @TemplateHaskell@.
--
-- @since 0.1.0.0
refineTH :: forall p a. (Predicate p a, Lift a) => a -> Q (TExp (Refined '[p] a))
refineTH x = case validate @p Proxy x of
  Nothing -> TH.TExp <$> TH.lift (UnsafeRefined x)
  Just err -> error $ "Error validating Predicate in mkRefinedTH: " <> show err

-- | Attempts to prove the given predicate. If it succeeds, we return the
-- refined @a@. Otherwise we die with a runtime error.
--
-- >>> unsafeRefine @NonNegative 0
-- UnsafeRefined {unrefine = 0}
--
-- @since 0.1.0.0
unsafeRefine :: forall p a. Predicate p a => a -> Refined '[p] a
unsafeRefine x = case validate @p Proxy x of
  Nothing -> UnsafeRefined x
  Just err -> error $ "Error validating Predicate in unsafeRefined: " <> show err

-- | Attempts to prove the given predicate. If it succeeds, we add the
-- predicate to the list.
--
-- >>> let x = unsafeRefine @NonNegative @Int 7
-- >>>     y = prove @NonZero x
-- >>> :type y
-- y :: Either
--        RefineException (Refined '[GreaterThanEq 0, NotEquals 0] Int)
--
-- >>> y
-- Right (UnsafeRefined {unrefine = 7})
--
-- >>> let z = prove @Even x
-- >>> z
-- Left (MkRefineException {predRep = Even, targetRep = Int, msg = "7 is not even"})
--
-- @since 0.1.0.0
prove :: forall p ps a. Predicate p a => Refined ps a -> Either RefineException (Refined (AppendP p ps) a)
prove (MkRefined x) = case validate @p Proxy x of
  Nothing -> Right $ UnsafeRefined x
  Just ex -> Left ex

-- | Attempts to prove the given predicate. If it succeeds, we add the
-- predicate to the list.
--
-- >>> let x = unsafeRefine @NonNegative @Int 7
-- >>>     y = unsafeProve @NonZero x
-- >>> :type y
-- y :: Refined '[GreaterThanEq 0, NotEquals 0] Int
--
-- >>> y
-- UnsafeRefined {unrefine = 7}
--
-- @since 0.1.0.0
unsafeProve :: forall p ps a. Predicate p a => Refined ps a -> Refined (AppendP p ps) a
unsafeProve (MkRefined x) = case validate @p Proxy x of
  Nothing -> UnsafeRefined x
  Just ex -> error $ show ex

-- | Removes all occurrences of the desired predicate, if it exists.
--
-- >>> let x = unsafeRefine @Even @Int 8
-- >>>     x' = relax @Even x
-- >>> :type x'
-- x' :: Refined '[] Int
--
-- @since 0.1.0.0
relax :: forall p ps a. Refined ps a -> Refined (DeleteP p ps) a
relax (MkRefined x) = UnsafeRefined x

-- | Removes all predicates.
--
-- >>> let x = unsafeRefine @Even @Int 8
-- >>>     y = unsafeProve @Positive x
-- >>> :type y
-- y :: Refined '[Even, GreaterThan 0] Int
--
-- >>> :type (relaxAll y)
-- (relaxAll y) :: Refined '[] Int
--
-- @since 0.1.0.0
relaxAll :: Refined ps a -> Refined '[] a
relaxAll (MkRefined x) = UnsafeRefined x

-- | @'Implies' ps p@ raises a type error if @p@ is not found
-- within @ps@.
--
-- >>> -- safeDiv :: (Implies ps NonZero, Integral n) => n -> Refined ps n -> n
-- >>> let d = unsafeRefine @NonZero @Int 7
-- >>> safeDiv 14 d
-- 2
--
-- @since 0.1.0.0
type Implies :: [Type] -> Type -> Constraint
type family Implies ps p where
  Implies ps p = ErrIfFalse (PredNotFound ps p) (ImpliesBool ps p)

-- | @'AppendP' p ps@ appends @p@ to @ps@ if it does not already occur
-- in the list.
--
-- @since 0.1.0.0
type AppendP :: Type -> [Type] -> [Type]
type family AppendP p ps where
  AppendP p '[] = '[p]
  AppendP p (p : ps) = p : ps
  AppendP p (q : ps) = q : AppendP p ps

-- | @'DeleteP' p ps@ deletes all occurrences of @p@ from @ps@.
--
-- @since 0.1.0.0
type DeleteP :: Type -> [Type] -> [Type]
type family DeleteP p ps where
  DeleteP _ '[] = '[]
  DeleteP p (p : ps) = DeleteP p ps
  DeleteP p (q : ps) = q : DeleteP p ps

-- | Variant of 'Implies' that returns a 'Bool'.
--
-- @since 0.1.0.0
type ImpliesBool :: [Type] -> Type -> Bool
type family ImpliesBool ps p where
  ImpliesBool '[] _ = 'False
  ImpliesBool (p : _) p = 'True
  ImpliesBool (q : ps) p = ImpliesBool ps p

-- | Emits a 'TypeError' when given 'False'. The parameter type is used in the
-- error message.
--
-- @since 0.1.0.0
type ErrIfFalse :: ErrorMessage -> Bool -> Constraint
type family ErrIfFalse p b where
  ErrIfFalse err 'False = TypeError err
  ErrIfFalse _ _ = ()

-- | Type error for a missing, wanted predicate.
--
-- @since 0.1.0.0
type PredNotFound :: [Type] -> Type -> ErrorMessage

type PredNotFound ps p =
  ( 'Text "Desired predicate "
      ':<>: 'ShowType p
      ':<>: 'Text " was not found in list "
      ':<>: 'ShowType ps
  )
