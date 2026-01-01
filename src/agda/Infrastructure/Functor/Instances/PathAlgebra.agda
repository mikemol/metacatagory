{-# OPTIONS --without-K #-}

-- | PathAlgebraFunctor: identity functor instance for any PathAlgebra,
-- exposing PathAlgebra as a CategoryLike and reusing the generic
-- IdentityFunctor. This anchors the functor interface in the adequacy layer.

module Infrastructure.Functor.Instances.PathAlgebra where

open import Agda.Primitive using (Level; _⊔_)
open import Agda.Builtin.Equality using (_≡_; refl)

open import Infrastructure.Axiom.Adequacy using (PathAlgebra)
open import Infrastructure.Functor.Interface
open import Infrastructure.Functor.Instances.Trivial

-- Minimal Lift to adjust universe levels for Hom
record Lift {ℓ ℓ'} (A : Set ℓ) : Set (ℓ ⊔ ℓ') where
  constructor lift
  field lower : A

------------------------------------------------------------------------
-- CategoryLike view of a PathAlgebra
------------------------------------------------------------------------

mkPA-CategoryLike : ∀ {ℓV ℓP} {V : Set ℓV} → PathAlgebra {ℓV} {ℓP} V → CategoryLike (Lift {ℓ = ℓV} {ℓ' = ℓP} V)
CategoryLike.Hom      (mkPA-CategoryLike PA) (lift a) (lift b) = PathAlgebra.Path PA a b
CategoryLike.id       (mkPA-CategoryLike PA) = PathAlgebra.id PA
CategoryLike._∘_      (mkPA-CategoryLike PA) {A = lift a} {B = lift b} {C = lift c} p q = PathAlgebra._++_ PA q p
CategoryLike.id-left  (mkPA-CategoryLike PA) {A = lift a} {B = lift b} p = PathAlgebra.id-right PA p
CategoryLike.id-right (mkPA-CategoryLike PA) {A = lift a} {B = lift b} p = PathAlgebra.id-left PA p
CategoryLike.assoc    (mkPA-CategoryLike PA) h g f = PathAlgebra.++-assoc PA f g h

------------------------------------------------------------------------
-- Identity functor for any PathAlgebra
------------------------------------------------------------------------

module PathAlgebraIdentity {ℓV ℓP : Level} {V : Set ℓV} (PA : PathAlgebra {ℓV} {ℓP} V) where
  private
    Cat = mkPA-CategoryLike PA
  open IdentityFunctor Cat public using () renaming (identity to identityPathAlgebra)
