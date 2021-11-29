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
    refine2,
    refine3,
    refine4,
    refine5,
    refineTH,
    refineTH2,
    refineTH3,
    refineTH4,
    refineTH5,
    unsafeRefine,

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
    type (<-/->),

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

import Control.Monad ((>=>))
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import Data.Type.Bool qualified as B
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Language.Haskell.TH.Syntax (Lift, Q, TExp)
import Language.Haskell.TH.Syntax qualified as TH
import Refined.Internal (RefineException (..), Refined (..))
import Refined.Predicate
  ( Not,
    Or,
    Predicate (..),
    Xor,
    type (<-/->),
    type (\/),
  )
import Refined.Predicate qualified as P
import Refined.Predicate.Foldable
import Refined.Predicate.Math
import Refined.Predicate.Text

-- $setup
-- >>> safeDiv :: (Implies ps NonZero, Integral n) => n -> Refined ps n -> n; safeDiv x d = x `div` unrefine d
-- >>> import Data.Text (Text)
-- >>> :set -XOverloadedStrings

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

-- | Attempts to prove two predicates. If it succeeds, we return the
-- refined @a@. Otherwise we return an error.
--
-- ==== __Examples__
-- >>> let x = refine2 @Positive @Odd @Int 5
-- >>> :type x
-- x :: Either RefineException (Refined '[GreaterThan 0, Odd] Int)
--
-- >>> x
-- Right (UnsafeRefined {unrefine = 5})
--
-- >>> refine2 @Positive @Even @Int 5
-- Left (MkRefineException {predRep = Even, targetRep = Int, msg = "5 is not even"})
--
-- @since 0.1.0.0
refine2 ::
  forall p q a.
  (Predicate p a, Predicate q a) =>
  a ->
  Either RefineException (Refined (AppendP q '[p]) a)
refine2 = refine @p >=> addPred @q

-- | Attempts to prove three predicates. If it succeeds, we return the
-- refined @a@. Otherwise we return an error.
--
-- ==== __Examples__
-- >>> let x = refine3 @Positive @Odd @(GreaterThan 2) @Int 5
-- >>> :type x
-- x :: Either
--        RefineException (Refined '[GreaterThan 0, Odd, GreaterThan 2] Int)
--
-- >>> x
-- Right (UnsafeRefined {unrefine = 5})
--
-- >>> refine3 @Positive @Odd @(GreaterThan 6) @Int 5
-- Left (MkRefineException {predRep = GreaterThan 6, targetRep = Int, msg = "5 does not satisfy > 6"})
--
-- @since 0.1.0.0
refine3 ::
  forall p q r a.
  (Predicate p a, Predicate q a, Predicate r a) =>
  a ->
  Either RefineException (Refined (AppendP r (AppendP q '[p])) a)
refine3 = refine2 @p @q >=> addPred @r

-- | Attempts to prove four predicates. If it succeeds, we return the
-- refined @a@. Otherwise we return an error.
--
-- @since 0.1.0.0
refine4 ::
  forall p q r s a.
  ( Predicate p a,
    Predicate q a,
    Predicate r a,
    Predicate s a
  ) =>
  a ->
  Either RefineException (Refined (AppendP s (AppendP r (AppendP q '[p]))) a)
refine4 = refine3 @p @q @r >=> addPred @s

-- | Attempts to prove five predicates. If it succeeds, we return the
-- refined @a@. Otherwise we return an error.
--
-- @since 0.1.0.0
refine5 ::
  forall p q r s t a.
  ( Predicate p a,
    Predicate q a,
    Predicate r a,
    Predicate s a,
    Predicate t a
  ) =>
  a ->
  Either
    RefineException
    (Refined (AppendP t (AppendP s (AppendP r (AppendP q '[p])))) a)
refine5 = refine4 @p @q @r @s >=> addPred @t

-- | Proves @p@ at compile-time via @TemplateHaskell@.
--
-- ==== __Examples__
-- @
--  $$(('refineTH' \@'NonZero' 5)) :: 'Refined' \'['NonZero'] 'Int'
-- @
--
-- @since 0.1.0.0
refineTH :: forall p a. (Predicate p a, Lift a) => a -> Q (TExp (Refined '[p] a))
refineTH x = case satisfies @p Proxy x of
  Nothing -> TH.TExp <$> TH.lift (UnsafeRefined x)
  Just err -> error $ "Error validating Predicate in refineTH: " <> show err

-- | Proves @p@ and @q@ at compile-time via @TemplateHaskell@.
--
-- ==== __Examples__
-- @
--  $$(('refineTH2' \@'NonZero' \@'Odd' 5)) :: 'Refined' '['NonZero', 'Odd'] 'Int'
-- @
--
-- @since 0.1.0.0
refineTH2 ::
  forall p q a.
  (Predicate p a, Predicate q a, Lift a) =>
  a ->
  Q (TExp (Refined (AppendP q '[p]) a))
refineTH2 x = case satisfies @p Proxy x *> satisfies @q Proxy x of
  Nothing -> TH.TExp <$> TH.lift (UnsafeRefined x)
  Just err -> error $ "Error validating Predicate in refineTH2: " <> show err

-- | Proves @p@ @q@, and @r@ at compile-time via @TemplateHaskell@.
--
-- ==== __Examples__
-- @
--  $$(('refineTH3' \@'NonZero' \@'Odd' @('GreaterThan' 4) 5)) :: 'Refined' '['NonZero', 'Odd', 'GreaterThan' 4] 'Int'
-- @
--
-- @since 0.1.0.0
refineTH3 ::
  forall p q r a.
  (Predicate p a, Predicate q a, Predicate r a, Lift a) =>
  a ->
  Q (TExp (Refined (AppendP r (AppendP q '[p])) a))
refineTH3 x = case satisfies @p Proxy x
  *> satisfies @q Proxy x
  *> satisfies @r Proxy x of
  Nothing -> TH.TExp <$> TH.lift (UnsafeRefined x)
  Just err -> error $ "Error validating Predicate in refineTH3: " <> show err

-- | Proves @p@, @q@, @r@, and @s@ at compile-time via @TemplateHaskell@.
--
-- @since 0.1.0.0
refineTH4 ::
  forall p q r s a.
  ( Predicate p a,
    Predicate q a,
    Predicate r a,
    Predicate s a,
    Lift a
  ) =>
  a ->
  Q (TExp (Refined (AppendP s (AppendP r (AppendP q '[p]))) a))
refineTH4 x = case satisfies @p Proxy x
  *> satisfies @q Proxy x
  *> satisfies @r Proxy x
  *> satisfies @s Proxy x of
  Nothing -> TH.TExp <$> TH.lift (UnsafeRefined x)
  Just err -> error $ "Error validating Predicate in refineTH4: " <> show err

-- | Proves @p@, @q@, @r@, @s@, and @t@ at compile-time via @TemplateHaskell@.
--
-- @since 0.1.0.0
refineTH5 ::
  forall p q r s t a.
  ( Predicate p a,
    Predicate q a,
    Predicate r a,
    Predicate s a,
    Predicate t a,
    Lift a
  ) =>
  a ->
  Q (TExp (Refined (AppendP t (AppendP s (AppendP r (AppendP q '[p])))) a))
refineTH5 x = case satisfies @p Proxy x
  *> satisfies @q Proxy x
  *> satisfies @r Proxy x
  *> satisfies @s Proxy x
  *> satisfies @t Proxy x of
  Nothing -> TH.TExp <$> TH.lift (UnsafeRefined x)
  Just err -> error $ "Error validating Predicate in refineTH5: " <> show err

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
  ImpliesBoolExpr (e1 <-/-> e2) p = ImpliesBoolExpr e1 p B.&& ImpliesBoolExpr e2 p
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
