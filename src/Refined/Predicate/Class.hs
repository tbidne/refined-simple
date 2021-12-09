{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides the 'Predicate' typeclass.
--
-- @since 0.1.0.0
module Refined.Predicate.Class
  ( Predicate (..),
    mkRefineException,
  )
where

import Data.Typeable (Proxy (..), Typeable)
import Data.Typeable qualified as Ty
import Refined.Internal (RefineException (..))

-- | This class is used for defining new predicates.
--
-- We include instances for type-level lists so that we can prove multiple
-- predicates simultaneously.
--
-- @since 0.1.0.0
class Predicate p a where
  -- | Validates predicate @p@ for the type @a@. Returns 'Nothing' on success,
  -- 'Just' 'RefineException' on a validation failure.
  --
  -- @since 0.1.0.0
  satisfies :: Proxy p -> a -> Maybe RefineException

-- | @mkRefineException \@p \@a msg@ creates a 'RefineException' for type @a@,
-- predicate @p@, and error message @msg@.
--
-- @since 0.1.0.0
mkRefineException :: forall p a. (Predicate p a, Typeable p, Typeable a) => String -> RefineException
mkRefineException = MkRefineException pTy aTy
  where
    pTy = Ty.typeRep (Proxy @p)
    aTy = Ty.typeRep (Proxy @a)

-- | @since 0.1.0.0
instance Predicate (Proxy '[]) a where
  satisfies _ _ = Nothing

-- | @since 0.1.0.0
instance (Predicate (Proxy ps) a, Predicate p a) => Predicate (Proxy (p ': ps)) a where
  satisfies _ x = case satisfies @p @a Proxy x of
    Nothing -> satisfies @(Proxy ps) Proxy x
    Just ex -> Just ex
