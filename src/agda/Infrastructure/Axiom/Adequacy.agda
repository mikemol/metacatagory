{-# OPTIONS --without-K #-}

-- | Adequacy layer: axioms as solvable parameters with path algebra scaffolding.
module Infrastructure.Axiom.Adequacy where

open import Agda.Primitive using (Level; _⊔_; lsuc)
open import Agda.Builtin.Equality using (_≡_)

-- This layer formalizes the project invariant:
-- "An axiom is a parameter whose associated theorems must be constructible from its generators."

------------------------------------------------------------------------
-- Minimal algebra of paths and 2-cells (kept abstract, provided as parameters)
------------------------------------------------------------------------

record PathAlgebra {ℓV ℓP : Level} (V : Set ℓV) : Set (lsuc (ℓV ⊔ ℓP)) where
  field
    Path   : V → V → Set (ℓV ⊔ ℓP)
    _++_   : ∀ {a b c} → Path a b → Path b c → Path a c
    ++-assoc : ∀ {a b c d}
      (p : Path a b) (q : Path b c) (r : Path c d) →
      (p ++ q) ++ r ≡ p ++ (q ++ r)
    id     : ∀ {a} → Path a a
    id-left  : ∀ {a b} (p : Path a b) → _++_ id p ≡ p
    id-right : ∀ {a b} (p : Path a b) → _++_ p id ≡ p

-- A 2-cell is equality of two parallel paths.
Cell₂ : ∀ {ℓV ℓP : Level} {V : Set ℓV} → (PA : PathAlgebra {ℓV} {ℓP} V) →
        ∀ (a b : V) → Set (lsuc (ℓV ⊔ ℓP))
Cell₂ {V = V} PA a b = let open PathAlgebra PA in Path a b ≡ Path a b

------------------------------------------------------------------------
-- Faces (axioms-to-be) are boundaries: two paths with same endpoints.
------------------------------------------------------------------------

record Face {ℓV ℓP : Level} {V : Set ℓV} (PA : PathAlgebra {ℓV} {ℓP} V)
            (a b : V) : Set (lsuc (ℓV ⊔ ℓP)) where
  open PathAlgebra PA
  field
    lhs rhs : Path a b

-- Adequacy means: the Face is solvable from a generator kit.
record Adequate
  {ℓV ℓP ℓG : Level} {V : Set ℓV}
  (PA : PathAlgebra {ℓV} {ℓP} V)
  (Generators : Set ℓG)
  (Solve : ∀ {a b} (F : Face PA a b) → Generators → Set (lsuc (ℓV ⊔ ℓP)))
  : Set (lsuc (ℓV ⊔ ℓP ⊔ ℓG)) where
  field
    -- "Generators suffice": for every face, a proof exists given the kit.
    adequate : ∀ {a b} (F : Face PA a b) (G : Generators) →
      Solve F G

-- Note: specific mechanisms (triangulations, whiskering, naturality) live in generator kits.
