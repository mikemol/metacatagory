module Infrastructure.Axiom.Solver where

open import Agda.Primitive using (Level; _⊔_; lsuc)
open import Agda.Builtin.Equality using (_≡_)
open import Infrastructure.Axiom.Adequacy using (PathAlgebra)
open import Infrastructure.Axiom.Face using (Face)

-- A generic "theorem generator": given a face and a kit, produce a 2-cell filling it.
record Solver
  {ℓV ℓP ℓK : Level} {V : Set ℓV}
  (PA : PathAlgebra {ℓV} {ℓP} V)
  (Kit : Set ℓK)
  : Set (lsuc (ℓV ⊔ ℓP ⊔ ℓK)) where
  open PathAlgebra PA
  field
    fill : ∀ {a b} (F : Face PA a b) → Kit → Face.lhs F ≡ Face.rhs F

-- A "triangulated" solver is one whose kit is a finite gluing basis (triangles/squares),
-- so higher faces (pentagon/hexagon/…) become derived theorems.
