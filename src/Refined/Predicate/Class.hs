{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Provides the 'Predicate' typeclass.
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
-- @since 0.1.0.0
class (Typeable p, Typeable a) => Predicate p a where
  -- | Validates predicate @p@ for the type @a@. Returns 'Nothing' on success,
  -- 'Just' 'RefineException' on a validation failure.
  --
  -- @since 0.1.0.0
  validate :: Proxy p -> a -> Maybe RefineException

-- | @mkRefineException \@p \@a msg@ creates a 'RefineException' for type @a@,
-- predicate @p@, and error message @msg@.
--
-- @since 0.1.0.0
mkRefineException :: forall p a. Predicate p a => String -> RefineException
mkRefineException = MkRefineException pTy aTy
  where
    pTy = Ty.typeRep (Proxy @p)
    aTy = Ty.typeRep (Proxy @a)