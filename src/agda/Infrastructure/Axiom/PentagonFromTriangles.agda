{-# OPTIONS --without-K #-}
module Infrastructure.Axiom.PentagonFromTriangles where

open import Agda.Primitive using (Level; _⊔_)
open import Agda.Builtin.Equality using (_≡_; refl)

open import Infrastructure.Axiom.SolvableInterface using (PathAlg; cong)
open import Infrastructure.Equality using (trans; sym)

-- | Whiskering: lift a local equality into a larger composite.
whisker
  : ∀ {ℓV ℓP} {V : Set ℓV}
    (PA : PathAlg {ℓV} {ℓP} V)
    {a b c d : V}
    (p : PathAlg.Path PA a b) (q : PathAlg.Path PA c d)
    {r s : PathAlg.Path PA b c}
    → r ≡ s
    → PathAlg._∙_ PA (PathAlg._∙_ PA p r) q ≡ PathAlg._∙_ PA (PathAlg._∙_ PA p s) q
whisker PA p q {r = r} {s = s} eq =
  let open PathAlg PA in
  trans (assoc p r q)
        (trans (cong (λ x → p ∙ x) (cong (λ x → x ∙ q) eq))
               (sym (assoc p s q)))

-- | Pentagon reduction using a single diagonal triangle filler.
pentagon-from-triangle
  : ∀ {ℓV ℓP} {V : Set ℓV}
    (PA : PathAlg {ℓV} {ℓP} V)
    {v0 v1 v2 v3 v4 : V}
    (e01 : PathAlg.Path PA v0 v1)
    (e12 : PathAlg.Path PA v1 v2)
    (e23 : PathAlg.Path PA v2 v3)
    (e34 : PathAlg.Path PA v3 v4)
    (e13 : PathAlg.Path PA v1 v3)               -- diagonal from triangulation
    (t1  : e13 ≡ PathAlg._∙_ PA e12 e23)        -- local triangle filler
  → PathAlg._∙_ PA (PathAlg._∙_ PA e01 e13) e34 ≡ PathAlg._∙_ PA (PathAlg._∙_ PA (PathAlg._∙_ PA e01 e12) e23) e34
pentagon-from-triangle PA e01 e12 e23 e34 e13 t1 =
  let open PathAlg PA in
  trans (cong (λ x → x ∙ e34) (cong (λ x → e01 ∙ x) t1))
       (cong (λ x → x ∙ e34) (sym (assoc e01 e12 e23)))
