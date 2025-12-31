{-# OPTIONS --without-K #-}
module Infrastructure.Axiom.SolvableInterface where

open import Agda.Primitive using (Level; _⊔_; lsuc)
open import Agda.Builtin.Equality using (_≡_; refl; cong)
open import Agda.Builtin.Sum using (_⊎_; inj₁; inj₂)

------------------------------------------------------------------------
-- Path algebra with explicit identities (kept minimal and reusable)
------------------------------------------------------------------------

record PathAlg {ℓV ℓP : Level} (V : Set ℓV) : Set (lsuc (ℓV ⊔ ℓP)) where
  field
    Path  : V → V → Set ℓP
    _∙_   : ∀ {a b c} → Path a b → Path b c → Path a c
    id    : ∀ {a} → Path a a
    assoc : ∀ {a b c d} (p : Path a b) (q : Path b c) (r : Path c d)
          → (p ∙ q) ∙ r ≡ p ∙ (q ∙ r)
    idl   : ∀ {a b} (p : Path a b) → id ∙ p ≡ p
    idr   : ∀ {a b} (p : Path a b) → p ∙ id ≡ p

open PathAlg public

------------------------------------------------------------------------
-- Faces (boundaries) and framed faces
------------------------------------------------------------------------

record Face {ℓV ℓP : Level} {V : Set ℓV} (PA : PathAlg {ℓV} {ℓP} V)
            (a b : V) : Set (ℓV ⊔ ℓP) where
  open PathAlg PA
  field lhs rhs : Path a b

record FramedFace {ℓV ℓP : Level} {V : Set ℓV} (PA : PathAlg {ℓV} {ℓP} V)
  : Set (lsuc (ℓV ⊔ ℓP)) where
  field
    a b  : V
    face : Face PA a b

------------------------------------------------------------------------
-- Solvers: constructive adequacy (total) and diagnostic (partial)
------------------------------------------------------------------------

record Solver {ℓV ℓP ℓK : Level} {V : Set ℓV}
              (PA : PathAlg {ℓV} {ℓP} V)
              (Kit : Set ℓK)
              : Set (lsuc (ℓV ⊔ ℓP ⊔ ℓK)) where
  open PathAlg PA
  field
    boundary : Kit → FramedFace PA
    solve    : (k : Kit) →
               let ff = boundary k in
               Face.lhs (FramedFace.face ff) ≡ Face.rhs (FramedFace.face ff)

data Obligation : Set where
  MissingTriangle : Obligation
  MissingWhisker  : Obligation
  MissingDiagonal : Obligation

record Solver? {ℓV ℓP ℓK : Level} {V : Set ℓV}
               (PA : PathAlg {ℓV} {ℓP} V)
               (Kit : Set ℓK)
               : Set (lsuc (ℓV ⊔ ℓP ⊔ ℓK)) where
  open PathAlg PA
  field
    boundary : Kit → FramedFace PA
    solve?   : (k : Kit) →
               let ff = boundary k in
               (Face.lhs (FramedFace.face ff) ≡ Face.rhs (FramedFace.face ff)) ⊎ Obligation
