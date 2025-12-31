module Infrastructure.Axiom.Instance where

open import Agda.Primitive using (Level; _⊔_; lsuc)
open import Agda.Builtin.Equality using (_≡_)

open import Infrastructure.Axiom.Adequacy using (PathAlgebra)
open import Infrastructure.Axiom.Face using (Face)

-- Framed face packages endpoints so instances need not juggle implicit indices.
record FramedFace {ℓV ℓP : Level} {V : Set ℓV}
                  (PA : PathAlgebra {ℓV} {ℓP} V)
  : Set (lsuc (ℓV ⊔ ℓP)) where
  field
    a b  : V
    face : Face PA a b

record AxiomInstance
  {ℓV ℓP ℓK : Level} {V : Set ℓV}
  (PA : PathAlgebra {ℓV} {ℓP} V)
  : Set (lsuc (ℓV ⊔ ℓP ⊔ ℓK)) where
  field
    Kit   : Set ℓK
    face  : Kit → FramedFace PA
    solve : (K : Kit) →
      let ff = face K in Face.lhs (FramedFace.face ff) ≡ Face.rhs (FramedFace.face ff)
