{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This is the main entry point to the library. It provides the core
-- types and functions for usage.
module Refined
  ( -- * Types

    -- ** Refined
    Refined (MkRefined, unrefine),
    refineEmpty,
    refine,
    refineAll,
    refineTH,
    refineAllTH,
    unsafeRefine,
    unsafeRefineAll,

    -- ** RefineException
    RefineException (..),
    P.mkRefineException,

    -- * Predicates
    Predicate (..),

    -- ** Operators
    Not,
    Or,
    type (\/),
    Xor,
    type (<+>),

    -- ** Proving
    addPred,
    unsafeAddPred,
    relax,
    relaxAll,

    -- ** TypeErrors
    Implies,
    PredNotFound,

    -- ** Built-In
    module Refined.Predicate.Math,
    module Refined.Predicate.Foldable,
    module Refined.Predicate.Text,

    -- * Misc Utils
    AppendP,
    DeleteP,
    ImpliesBool,
    ImpliesBoolExpr,
    ErrIfFalse,
  )
where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import Data.Type.Bool qualified as B
import GHC.TypeLits (ErrorMessage (..), TypeError)
#if MIN_VERSION_template_haskell(2, 17, 0)
import Language.Haskell.TH.Syntax (Code, Lift (..), Q)
#else
import Language.Haskell.TH.Syntax (Lift (..), Q, TExp)
#endif
import Refined.Internal (RefineException (..), Refined (..))
import Refined.Predicate
  ( Not,
    Or,
    Predicate (..),
    Xor,
    type (<+>),
    type (\/),
  )
import Refined.Predicate qualified as P
import Refined.Predicate.Foldable
import Refined.Predicate.Math
import Refined.Predicate.Text

-- $setup
-- >>> safeDiv :: (Implies ps NonZero, Integral n) => n -> Refined ps n -> n; safeDiv x d = x `div` unrefine d
-- >>> import Data.Text (Text)
-- >>> :set -XTemplateHaskell

-- | Wraps @a@ in 'Refined' with no attached predicates.
--
-- ==== __Examples__
-- >>> refineEmpty 0
-- UnsafeRefined {unrefine = 0}
--
-- @since 0.1.0.0
refineEmpty :: a -> Refined '[] a
refineEmpty = UnsafeRefined

-- | Attempts to prove the given predicate. If it succeeds, we return the
-- refined @a@. Otherwise we return an error.
--
-- ==== __Examples__
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
refine x = case satisfies @p Proxy x of
  Nothing -> Right $ UnsafeRefined x
  Just ex -> Left ex

-- | Attempts to prove multiple predicates. Returns the first error
-- encountered, if any.
--
-- ==== __Examples__
-- >>> let x = refineAll @'[Positive, Odd, (GreaterThan 2)] @Int 5
-- >>> :type x
-- x :: Either
--        RefineException (Refined '[Positive, Odd, GreaterThan 2] Int)
--
-- >>> x
-- Right (UnsafeRefined {unrefine = 5})
--
-- >>> refineAll @'[Positive, Odd, GreaterThan 6] @Int 5
-- Left (MkRefineException {predRep = GreaterThan 6, targetRep = Int, msg = "5 does not satisfy > 6"})
--
--
-- @since 0.1.0.0
refineAll :: forall ps a. (Predicate (Proxy ps) a) => a -> Either RefineException (Refined ps a)
refineAll x = case satisfies @(Proxy ps) @a Proxy x of
  Nothing -> Right $ UnsafeRefined x
  Just ex -> Left ex

-- | Proves @p@ at compile-time via @TemplateHaskell@.
--
-- ==== __Examples__
-- >>> let x = $$((refineTH @NonZero 5))
-- >>> :type x
-- x :: Refined '[NonZero] Integer
--
-- >>> x
-- UnsafeRefined {unrefine = 5}
--
-- @since 0.1.0.0
#if MIN_VERSION_template_haskell(2, 17, 0)
refineTH :: forall p a. (Predicate p a, Lift a) => a -> Code Q (Refined '[p] a)
#else
refineTH :: forall p a. (Predicate p a, Lift a) => a -> Q (TExp (Refined '[p] a))
#endif
refineTH x = case satisfies @p Proxy x of
  Nothing -> liftTyped (UnsafeRefined x)
  Just err -> error $ "Error validating Predicate in refineTH: " <> show err

-- | Proves multiple predicates at compile-time via @TemplateHaskell@.
-- See 'Refined.refineAll'.
--
-- ==== __Examples__
-- >>> let x = $$((refineAllTH @[NonZero, Odd, Positive] 5))
-- >>> :type x
-- x :: Refined '[NonZero, Odd, Positive] Integer
--
-- >>> x
-- UnsafeRefined {unrefine = 5}
--
-- @since 0.1.0.0
#if MIN_VERSION_template_haskell(2, 17, 0)
refineAllTH :: forall ps a. (Predicate (Proxy ps) a, Lift a) => a -> Code Q (Refined ps a)
#else
refineAllTH :: forall ps a. (Predicate (Proxy ps) a, Lift a) => a -> Q (TExp (Refined ps a))
#endif
refineAllTH x = case satisfies @(Proxy ps) @a Proxy x of
  Nothing -> liftTyped (UnsafeRefined x)
  Just err -> error $ "Error validating Predicate in refineAllTH: " <> show err

-- | Attempts to prove the given predicate. If it succeeds, we return the
-- refined @a@. Otherwise we die with a runtime error.
--
-- >>> unsafeRefine @NonNegative 0
-- UnsafeRefined {unrefine = 0}
--
-- @since 0.1.0.0
unsafeRefine :: forall p a. Predicate p a => a -> Refined '[p] a
unsafeRefine x = case satisfies @p Proxy x of
  Nothing -> UnsafeRefined x
  Just err -> error $ "Error validating Predicate in unsafeRefined: " <> show err

-- | Attempts to prove the given predicates. If it succeeds, we return the
-- refined @a@. Otherwise we die with a runtime error.
--
-- >>> unsafeRefineAll @'[NonNegative, Even] 4
-- UnsafeRefined {unrefine = 4}
--
-- @since 0.1.0.0
unsafeRefineAll :: forall ps a. (Predicate (Proxy ps) a) => a -> Refined ps a
unsafeRefineAll x = case satisfies @(Proxy ps) Proxy x of
  Nothing -> UnsafeRefined x
  Just err -> error $ "Error validating Predicate in unsafeRefineAll: " <> show err

-- | Attempts to prove the given predicate. If it succeeds, we add the
-- predicate to the list.
--
-- ==== __Examples__
-- >>> let x = unsafeRefine @NonNegative @Int 7
-- >>>     y = addPred @NonZero x
-- >>> :type y
-- y :: Either
--        RefineException (Refined '[GreaterThanEq 0, Not (NatEquals 0)] Int)
--
-- >>> y
-- Right (UnsafeRefined {unrefine = 7})
--
-- >>> let z = addPred @Even x
-- >>> z
-- Left (MkRefineException {predRep = Even, targetRep = Int, msg = "7 is not even"})
--
-- @since 0.1.0.0
addPred :: forall p ps a. Predicate p a => Refined ps a -> Either RefineException (Refined (AppendP p ps) a)
addPred (MkRefined x) = case satisfies @p Proxy x of
  Nothing -> Right $ UnsafeRefined x
  Just ex -> Left ex

-- | Attempts to prove the given predicate. If it succeeds, we add the
-- predicate to the list.
--
-- ==== __Examples__
-- >>> let x = unsafeRefine @NonNegative @Int 7
-- >>>     y = unsafeAddPred @NonZero x
-- >>> :type y
-- y :: Refined '[GreaterThanEq 0, Not (NatEquals 0)] Int
--
-- >>> y
-- UnsafeRefined {unrefine = 7}
--
-- @since 0.1.0.0
unsafeAddPred :: forall p ps a. Predicate p a => Refined ps a -> Refined (AppendP p ps) a
unsafeAddPred (MkRefined x) = case satisfies @p Proxy x of
  Nothing -> UnsafeRefined x
  Just ex -> error $ show ex

-- | Removes all occurrences of the desired predicate, if it exists.
--
-- ==== __Examples__
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
-- ==== __Examples__
-- >>> let x = unsafeRefine @Even @Int 8
-- >>>     y = unsafeAddPred @Positive x
-- >>> :type y
-- y :: Refined '[Even, GreaterThan 0] Int
--
-- >>> :type (relaxAll y)
-- (relaxAll y) :: Refined '[] Int
--
-- @since 0.1.0.0
relaxAll :: Refined ps a -> Refined '[] a
relaxAll (MkRefined x) = UnsafeRefined x

-- | @'Implies' ps p@ raises a type error if @ps@ does not logically imply
-- @p@. See 'ImpliesBoolExpr' for more information on when @ps@ implies @p@.
--
-- ==== __Examples__
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
  ImpliesBool (q : ps) p = ImpliesBoolExpr q p B.|| ImpliesBool ps p

-- | @ImpliesBoolExpr expr p@ returns 'True if @expr@ logically implies @p@.
-- @expr@ is assumed to be a combination of predicate literals (i.e. a
-- predicate with no logical connectives) with logical combinators from
-- 'Refined.Predicate.Operators'.
--
-- NB. For a (possibly exclusive) disjunction to imply @p@, /both/ clauses
-- must individually imply @p@, since we cannot know which one is satisfied.
-- That is,
--
-- \[
--   q_1 \vee q_2 \implies p \quad \iff \quad (q_1 \implies p) \wedge (q_2 \implies p)
-- \]
--
-- ==== __Examples__
-- >>> :kind! ImpliesBoolExpr (Not NonZero) (NonNegative)
-- ImpliesBoolExpr (Not NonZero) (NonNegative) :: Bool
-- = 'False
--
-- >>> :kind! ImpliesBoolExpr (Not (Not NonNegative)) NonNegative
-- ImpliesBoolExpr (Not (Not NonNegative)) NonNegative :: Bool
-- = 'True
--
-- >>> :kind! ImpliesBoolExpr (NonZero \/ Alpha) NonZero
-- ImpliesBoolExpr (NonZero \/ Alpha) NonZero :: Bool
-- = 'False
--
-- >>> :kind! ImpliesBoolExpr (Alpha \/ (Not (Not Alpha))) Alpha
-- ImpliesBoolExpr (Alpha \/ (Not (Not Alpha))) Alpha :: Bool
-- = 'True
--
-- @since 0.1.0.0
type ImpliesBoolExpr :: Type -> Type -> Bool
type family ImpliesBoolExpr expr p where
  ImpliesBoolExpr p p = 'True
  ImpliesBoolExpr (Not p) p = 'False
  ImpliesBoolExpr (Not expr) p = ImpliesBoolExpr expr (Not p)
  ImpliesBoolExpr (e1 \/ e2) p = ImpliesBoolExpr e1 p B.&& ImpliesBoolExpr e2 p
  ImpliesBoolExpr (e1 <+> e2) p = ImpliesBoolExpr e1 p B.&& ImpliesBoolExpr e2 p
  ImpliesBoolExpr q p = 'False

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
