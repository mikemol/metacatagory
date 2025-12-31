module Infrastructure.Polytopes.Associahedron.Triangulation where

open import Agda.Primitive using (Level; lsuc; _⊔_)
open import Agda.Builtin.Equality using (_≡_; refl)

private
  congTri : ∀ {ℓ ℓ'} {A : Set ℓ} {B : Set ℓ'} {x y : A} (f : A → B) → x ≡ y → f x ≡ f y
  congTri f refl = refl

  symTri : ∀ {ℓ} {A : Set ℓ} {x y : A} → x ≡ y → y ≡ x
  symTri refl = refl

  transTri : ∀ {ℓ} {A : Set ℓ} {x y z : A} → x ≡ y → y ≡ z → x ≡ z
  transTri refl eq = eq

-- This module is intentionally generic: it expresses the *pattern* that
-- a higher coherence cell (pentagon boundary equality) can be obtained
-- by gluing triangle fillers along a chosen triangulation (fan).

record Triangle {ℓV ℓP : Level} {V : Set ℓV}
                (PathV : V → V → Set ℓP)
                (_++_  : ∀ {x y z} → PathV x y → PathV y z → PathV x z)
                (u v w : V) : Set (ℓV ⊔ ℓP) where
  field
    top  : PathV u v
    bot  : PathV v w
    diag : PathV u w
    fill : (top ++ bot) ≡ diag

-- A pentagon face typically yields two boundary paths between the same endpoints.
-- A "fan" triangulation provides a diagonal and two triangle fillers that allow
-- us to derive the boundary equality by composition.
record PentagonFan {ℓV ℓP : Level} {V : Set ℓV}
                   (PathV : V → V → Set ℓP)
                   (_++_  : ∀ {x y z} → PathV x y → PathV y z → PathV x z)
                   : Set (lsuc (ℓV ⊔ ℓP)) where
  field
    v0 v1 v2 v3 : V
    -- boundary edges
    e01 : PathV v0 v1
    e12 : PathV v1 v2
    e23 : PathV v2 v3

    -- two competing composites from v0 to v3
    pShort pLong : PathV v0 v3

    -- explicit expansions (avoid relying on definitional equality)
    shortDef : pShort ≡ (e01 ++ (e12 ++ e23))
    longDef  : pLong  ≡ ((e01 ++ e12) ++ e23)

    -- diagonal and triangle filler (top++bot = diag)
    d02 : PathV v0 v2
    tri0 : (e01 ++ e12) ≡ d02
    tri1 : (d02 ++ e23) ≡ pShort

-- Given associativity of ++, the short and long expansions coincide; the
-- triangles allow us to rewrite through the chosen diagonal.

pentagon-from-fan
  : ∀ {ℓV ℓP : Level} {V : Set ℓV}
    {PathV : V → V → Set ℓP}
    {_++_ : ∀ {x y z} → PathV x y → PathV y z → PathV x z}
    (assoc++ : ∀ {w x y z} (p : PathV w x) (q : PathV x y) (r : PathV y z)
             → (p ++ q) ++ r ≡ p ++ (q ++ r))
    (F : PentagonFan PathV _++_)
  → PentagonFan.pShort F ≡ PentagonFan.pLong F
pentagon-from-fan assoc++ F =
  let open PentagonFan F in
  transTri shortDef
    (transTri (symTri (assoc++ e01 e12 e23))
      (symTri longDef))
