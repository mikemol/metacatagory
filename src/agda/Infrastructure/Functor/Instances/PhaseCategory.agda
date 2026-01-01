{-# OPTIONS --without-K #-}

-- | PhaseCategory functor instances: identity functor for Core.Phase
-- expressed via the generic CategoryLike/IdentityFunctor machinery.

module Infrastructure.Functor.Instances.PhaseCategory where

open import Agda.Primitive using (Level)
open import Infrastructure.Functor.Interface
open import Infrastructure.Functor.Instances.Trivial
open import Core.PhaseCategoryWrapper using (PhaseCategoryLike)

module PhaseIdentity {ℓ : Level} where
  open IdentityFunctor (PhaseCategoryLike {ℓ}) public
    using () renaming (identity to phaseIdentityFunctor)
