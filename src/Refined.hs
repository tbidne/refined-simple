{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Main entry point to the library.
module Refined
  ( -- * Types
    Refined (MkRefined, unrefined),
    RefineException (..),

    -- * Predicate Class
    Predicate (..),

    -- * Creation
    refineEmpty,
    refine,
    refineTH,
    unsafeRefine,

    -- * Proving Predicates
    prove,
    relax,
    relaxAll,

    -- * Predicate Constraints
    Implies,

    -- * Predicate Utils
    AppendP,
    DeleteP,
    ImpliesBool,
    ErrIfFalse,

    -- * Type Errors
    PredNotFound,
  )
where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Language.Haskell.TH.Syntax (Lift, Q, TExp)
import Language.Haskell.TH.Syntax qualified as TH

-- | Newtype over @a@ that has a type-level list of attached predicates.
--
-- @since 0.1.0.0
type Refined :: [Type] -> Type -> Type
newtype Refined p a = UnsafeRefined
  { -- | @since 0.1.0.0
    unrefined :: a
  }
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Ord
    )
    via a
  deriving stock
    ( -- | @since 0.1.0.0
      Show
    )

-- | @since 0.1.0.0
instance (Lift x) => Lift (Refined p x) where
  liftTyped (UnsafeRefined a) = [||UnsafeRefined a||]

-- | @since 0.1.0.0
pattern MkRefined :: a -> Refined p a
pattern MkRefined a <- UnsafeRefined a

{-# COMPLETE MkRefined #-}

-- | @since 0.1.0.0
data RefineException = MkRefineException
  { -- | @since 0.1.0.0
    typeName :: String,
    -- | @since 0.1.0.0
    predName :: String,
    -- | @since 0.1.0.0
    msg :: String
  }
  deriving stock
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Ord,
      -- | @since 0.1.0.0
      Show
    )

-- | This class is used for defining new predicates.
--
-- @since 0.1.0.0
class Predicate p a where
  -- | Validates predicate @p@ for the type @a@. Returns 'Nothing' on success,
  -- 'Just' 'RefineException' on a validation failure.
  --
  -- @since 0.1.0.0
  validate :: Proxy p -> a -> Maybe RefineException

-- | Wraps @a@ in 'Refined' with no attached predicates.
--
-- @since 0.1.0.0
refineEmpty :: a -> Refined '[] a
refineEmpty = UnsafeRefined

-- | Attempts to prove the given predicate. If it succeeds, we return the
-- refined @a@. Otherwise we return an error.
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
-- @since 0.1.0.0
unsafeRefine :: forall p a. Predicate p a => a -> Refined '[p] a
unsafeRefine x = case validate @p Proxy x of
  Nothing -> UnsafeRefined x
  Just err -> error $ "Error validating Predicate in unsafeRefined: " <> show err

-- | Attempts to prove the given predicate. If it succeeds, we add the
-- predicate to the list.
--
-- @since 0.1.0.0
prove :: forall p ps a. Predicate p a => Refined ps a -> Either RefineException (Refined (AppendP p ps) a)
prove (MkRefined x) = case validate @p Proxy x of
  Nothing -> Right $ UnsafeRefined x
  Just ex -> Left ex

-- | Removes the desired predicate, if it exists.
--
-- @since 0.1.0.0
relax :: Refined ps a -> Refined (DeleteP p ps) a
relax (MkRefined x) = UnsafeRefined x

-- | Removes all predicates.
--
-- @since 0.1.0.0
relaxAll :: Refined ps a -> Refined '[] a
relaxAll (MkRefined x) = UnsafeRefined x

-- | @'Implies' ps p@ raises a type error if @p@ is not found
-- within @ps@.
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
      ':<>: 'Text " was not found in list"
      ':<>: 'ShowType ps
  )
