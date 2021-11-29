-- | Provides logical operators for combining predicates.
--
-- @since 0.1.0.0
module Refined.Predicate.Operators
  ( Not,
    Or,
    type (\/),
    Xor,
    type (<-/->),
  )
where

import Control.Applicative qualified as A
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Typeable qualified as Ty
import Refined.Predicate.Class (Predicate (..))
import Refined.Predicate.Class qualified as PC

-- $setup
-- >>> import Refined.Predicate.Math

-- | Logical negation.
--
-- ==== __Examples__
-- >>> satisfies (Proxy @(Not NonZero)) 0
-- Nothing
--
-- >>> satisfies (Proxy @(Not NonZero)) 7
-- Just (MkRefineException {predRep = Not (Not (NatEquals 0)), targetRep = Integer, msg = "7 does not satisfy (Not (Not (NatEquals 0)))"})
--
-- @since 0.1.0.0
type Not :: Type -> Type
data Not p

-- | @since 0.1.0.0
instance (Predicate p a, Show a) => Predicate (Not p) a where
  satisfies _ x = case satisfies (Proxy @p) x of
    Nothing -> Just $ PC.mkRefineException @(Not p) @a err
    Just _ -> Nothing
    where
      pTy = Ty.typeRep (Proxy @p)
      err = show x <> " does not satisfy (Not (" <> show pTy <> "))"

-- | Logical disjunction.
--
-- ==== __Examples__
-- >>> satisfies @(Or NonZero Positive) Proxy (-2)
-- Nothing
--
-- >>> satisfies @(Or NonZero Positive) Proxy 7
-- Nothing
--
-- >>> satisfies @(Or NonZero Positive) Proxy 0
-- Just (MkRefineException {predRep = Or (Not (NatEquals 0)) (GreaterThan 0), targetRep = Integer, msg = "0 satisfied neither predicate"})
--
-- @since 0.1.0.0
type Or :: Type -> Type -> Type
data Or p q

-- | @since 0.1.0.0
instance (Predicate p a, Predicate q a, Show a) => Predicate (Or p q) a where
  satisfies _ x = case A.liftA2 (,) satP satQ of
    Just _ -> Just $ PC.mkRefineException @(Or p q) @a err
    _ -> Nothing
    where
      satP = satisfies (Proxy @p) x
      satQ = satisfies (Proxy @q) x
      err = show x <> " satisfied neither predicate"

-- | Infix operator for 'Or'.
--
-- @since 0.1.0.0
type (\/) :: Type -> Type -> Type

type (\/) = Or

-- | Logical exclusive disjunction.
--
-- ==== __Examples__
-- >>> satisfies @(Xor NonZero Positive) Proxy (-2)
-- Nothing
--
-- >>> satisfies @(Xor NonZero Positive) Proxy 7
-- Just (MkRefineException {predRep = Xor (Not (NatEquals 0)) (GreaterThan 0), targetRep = Integer, msg = "7 satisfied both predicates"})
--
-- >>> satisfies @(Xor NonZero Positive) Proxy 0
-- Just (MkRefineException {predRep = Xor (Not (NatEquals 0)) (GreaterThan 0), targetRep = Integer, msg = "0 satisfied neither predicate"})
--
-- @since 0.1.0.0
type Xor :: Type -> Type -> Type
data Xor p q

-- | @since 0.1.0.0
instance (Predicate p a, Predicate q a, Show a) => Predicate (Xor p q) a where
  satisfies _ x = case (,) satP satQ of
    (Just _, Just _) -> Just $ PC.mkRefineException @(Xor p q) @a noneErr
    (Nothing, Nothing) -> Just $ PC.mkRefineException @(Xor p q) @a bothErr
    _ -> Nothing
    where
      satP = satisfies (Proxy @p) x
      satQ = satisfies (Proxy @q) x
      bothErr = show x <> " satisfied both predicates"
      noneErr = show x <> " satisfied neither predicate"

-- | Infix operator for 'Xor'.
--
-- @since 0.1.0.0
type (<-/->) :: Type -> Type -> Type

type (<-/->) = Xor
