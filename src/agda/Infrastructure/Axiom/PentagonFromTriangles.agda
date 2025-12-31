{-# OPTIONS --without-K #-}
module Infrastructure.Axiom.PentagonFromTriangles where

open import Agda.Primitive using (Level; _⊔_)
open import Agda.Builtin.Equality using (_≡_; refl; sym; cong)
open import Infrastructure.Axiom.SolvableInterface

-- Whiskering helper: lift a local equality into a larger context.
whisker
  : ∀ {ℓV ℓP} {V : Set ℓV}
    (PA : PathAlg {ℓV} {ℓP} V)
    {a b c d : V}
    (p : Path PA a b) (q : Path PA c d)
    {m n : V} {r s : Path PA b c}
    → r ≡ s
    → p ∙ r ∙ q ≡ p ∙ s ∙ q
whisker PA p q eq =
  let open PathAlg PA in
  trans (assoc p r q)
        (trans (cong (λ x → p ∙ x) (cong (λ x → x ∙ q) eq))
               (sym (assoc p s q)))
  where
    open PathAlg PA
    trans : ∀ {A : Set _} {x y z : A} → x ≡ y → y ≡ z → x ≡ z
    trans refl eq = eq

------------------------------------------------------------------------
-- Sample pentagon reduction via a single diagonal triangle
------------------------------------------------------------------------

pentagon-from-triangle
  : ∀ {ℓV ℓP} {V : Set ℓV}
    (PA : PathAlg {ℓV} {ℓP} V)
    {v0 v1 v2 v3 v4 : V}
    (e01 : Path PA v0 v1)
    (e12 : Path PA v1 v2)
    (e23 : Path PA v2 v3)
    (e34 : Path PA v3 v4)
    (e13 : Path PA v1 v3)               -- diagonal from triangulation
    (t1  : e13 ≡ e12 ∙ e23)             -- local triangle filler
  → (e01 ∙ e13 ∙ e34) ≡ (e01 ∙ e12 ∙ e23 ∙ e34)
pentagon-from-triangle PA e01 e12 e23 e34 e13 t1 =
  let open PathAlg PA in
  trans (assoc e01 e13 e34)
       (trans (cong (λ x → e01 ∙ x) (cong (λ x → x ∙ e34) t1))
              (sym (assoc e01 (e12 ∙ e23) e34)))
