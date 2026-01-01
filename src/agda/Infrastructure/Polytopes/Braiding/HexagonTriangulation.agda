{-# OPTIONS --allow-unsolved-metas #-}

-- | Hexagon triangulations for braiding coherence.
module Infrastructure.Polytopes.Braiding.HexagonTriangulation where

open import Agda.Primitive using (Level; lsuc; _⊔_)
open import Agda.Builtin.Equality using (_≡_; refl)

private
  congHex : ∀ {ℓ ℓ'} {A : Set ℓ} {B : Set ℓ'} {x y : A} (f : A → B) → x ≡ y → f x ≡ f y
  congHex f refl = refl

  transHex : ∀ {ℓ} {A : Set ℓ} {x y z : A} → x ≡ y → y ≡ z → x ≡ z
  transHex refl eq = eq

  symHex : ∀ {ℓ} {A : Set ℓ} {x y : A} → x ≡ y → y ≡ x
  symHex refl = refl

-- Triangulated hexagon coherence (shape-first).
--
-- We work in an abstract path algebra (V, PathV, ++), so this module can be
-- applied to concrete braid+assoc graphs via an adapter.

-- Congruence helpers for concatenation.
whiskerL++
  : ∀ {ℓ : Level} {V : Set ℓ}
    {PathV : V → V → Set ℓ}
    {_++_ : ∀ {x y z} → PathV x y → PathV y z → PathV x z}
    {x y z : V} {p p' : PathV x y} (q : PathV y z)
  → p ≡ p' → (p ++ q) ≡ (p' ++ q)
whiskerL++ { _++_ = _⊕_ } q e = congHex (λ p → _⊕_ p q) e

whiskerR++
  : ∀ {ℓ : Level} {V : Set ℓ}
    {PathV : V → V → Set ℓ}
    {_++_ : ∀ {x y z} → PathV x y → PathV y z → PathV x z}
    {x y z : V} (p : PathV x y) {q q' : PathV y z}
  → q ≡ q' → (p ++ q) ≡ (p ++ q')
whiskerR++ { _++_ = _⊕_ } p e = congHex (λ q → _⊕_ p q) e

-- A hexagon fan triangulation: choose two diagonals from v0 to v2 and v4,
-- then a shared diagonal v0→v3. Two triangles fill each side.
record HexagonFan {ℓV ℓP : Level} {V : Set ℓV}
                  (PathV : V → V → Set ℓP)
                  (_++_  : ∀ {x y z} → PathV x y → PathV y z → PathV x z)
                  : Set (lsuc (ℓV ⊔ ℓP)) where
  field
    v0 v1 v2 v3 v4 v5 : V

    -- left boundary chain v0→v1→v2→v3
    e01 : PathV v0 v1
    e12 : PathV v1 v2
    e23 : PathV v2 v3

    -- right boundary chain v0→v5→v4→v3
    e05 : PathV v0 v5
    e54 : PathV v5 v4
    e43 : PathV v4 v3

    -- boundary composites (the two competing paths from v0 to v3)
    pLeft  pRight : PathV v0 v3

    leftDef  : pLeft  ≡ (e01 ++ (e12 ++ e23))
    rightDef : pRight ≡ (e05 ++ (e54 ++ e43))

    -- diagonals + triangle fillers for the fan
    d02 : PathV v0 v2
    d04 : PathV v0 v4
    d03 : PathV v0 v3

    triL0 : (e01 ++ e12) ≡ d02
    triL1 : (d02 ++ e23) ≡ d03

    triR0 : (e05 ++ e54) ≡ d04
    triR1 : (d04 ++ e43) ≡ d03

postulate
  left-to-diagonal
    : ∀ {ℓ : Level} {V : Set ℓ}
      {PathV : V → V → Set ℓ}
      {_++_ : ∀ {x y z} → PathV x y → PathV y z → PathV x z}
      (assoc++ : ∀ {w x y z} (p : PathV w x) (q : PathV x y) (r : PathV y z)
               → (p ++ q) ++ r ≡ p ++ (q ++ r))
      (F : HexagonFan PathV _++_)
    → HexagonFan.pLeft F ≡ HexagonFan.d03 F

  right-to-diagonal
    : ∀ {ℓ : Level} {V : Set ℓ}
      {PathV : V → V → Set ℓ}
      {_++_ : ∀ {x y z} → PathV x y → PathV y z → PathV x z}
      (assoc++ : ∀ {w x y z} (p : PathV w x) (q : PathV x y) (r : PathV y z)
               → (p ++ q) ++ r ≡ p ++ (q ++ r))
      (F : HexagonFan PathV _++_)
    → HexagonFan.pRight F ≡ HexagonFan.d03 F

  hexagon-from-fan
    : ∀ {ℓ : Level} {V : Set ℓ}
      {PathV : V → V → Set ℓ}
      {_++_ : ∀ {x y z} → PathV x y → PathV y z → PathV x z}
      (assoc++ : ∀ {w x y z} (p : PathV w x) (q : PathV x y) (r : PathV y z)
               → (p ++ q) ++ r ≡ p ++ (q ++ r))
      (F : HexagonFan PathV _++_)
    → HexagonFan.pLeft F ≡ HexagonFan.pRight F
