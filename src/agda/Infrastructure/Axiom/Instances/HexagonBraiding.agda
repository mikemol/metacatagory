-- | Axiom instance for hexagon braiding coherence.
module Infrastructure.Axiom.Instances.HexagonBraiding where

open import Agda.Primitive using (Level; _⊔_)
open import Infrastructure.Axiom.Adequacy using (PathAlgebra)
open import Infrastructure.Axiom.Instance using (AxiomInstance; FramedFace)
open import Infrastructure.Polytopes.Braiding.HexagonTriangulation using (HexagonFan; hexagon-from-fan)

HexagonInstance
  : ∀ {ℓ : Level} {V : Set ℓ}
    (PA : PathAlgebra {ℓ} {ℓ} V)
  → AxiomInstance PA
HexagonInstance {ℓ} PA =
  let open PathAlgebra PA in
  record
    { Kit  = HexagonFan {ℓV = ℓ} {ℓP = ℓ} Path _++_
    ; face = λ K →
        let open HexagonFan K in
        -- Hexagon face assembled from the fan vertices.
        record { a = v0 ; b = v3 ; face = record { lhs = pLeft ; rhs = pRight } }
    ; solve = λ K → hexagon-from-fan (++-assoc) K
    }
