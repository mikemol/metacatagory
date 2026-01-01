{-# OPTIONS --without-K #-}

-- | 2-cell “movie” proofs: explicit equality witnesses between paths.
module Infrastructure.Axiom.Movie where

open import Agda.Primitive using (Level; _⊔_; lsuc)
open import Agda.Builtin.Equality using (_≡_; refl)
open import Infrastructure.Axiom.SolvableInterface using (PathAlg; Path; _∙_; Face)
open import Infrastructure.Equality using (trans)

------------------------------------------------------------------------
-- 2-cells as explicit equality witnesses between paths
------------------------------------------------------------------------

record Cell2 {ℓV ℓP : Level} {V : Set ℓV}
             (PA : PathAlg {ℓV} {ℓP} V)
             {a b : V}
             (p q : Path PA a b)
             : Set (ℓV ⊔ ℓP) where
  field proof : p ≡ q

------------------------------------------------------------------------
-- Movie: compositional traces of 2-cells
------------------------------------------------------------------------

data Movie {ℓV ℓP : Level} {V : Set ℓV} (PA : PathAlg {ℓV} {ℓP} V)
          : ∀ {a b} → (p q : Path PA a b) → Set (lsuc (ℓV ⊔ ℓP)) where
  step  : ∀ {a b} {p q : Path PA a b} → Cell2 PA p q → Movie PA p q
  vcomp : ∀ {a b} {p q r : Path PA a b} → Movie PA p q → Movie PA q r → Movie PA p r

------------------------------------------------------------------------
-- Interpret a movie back into an equality proof
------------------------------------------------------------------------

interp : ∀ {ℓV ℓP} {V : Set ℓV} {PA : PathAlg {ℓV} {ℓP} V} {a b}
       → {p q : Path PA a b} → Movie PA p q → p ≡ q
interp (step c)      = Cell2.proof c
interp (vcomp m n)   = trans (interp m) (interp n)
