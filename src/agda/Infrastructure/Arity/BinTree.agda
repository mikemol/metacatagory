-- | Binary tree arity utilities.
module Infrastructure.Arity.BinTree where

open import Agda.Primitive using (Level)
open import Agda.Builtin.Nat using (Nat; zero; suc; _+_)
open import Agda.Builtin.Equality using (_≡_; refl)

-- Local equality helpers (kept private to avoid export clashes)

private
  congEq : ∀ {ℓ ℓ'} {A : Set ℓ} {B : Set ℓ'} {x y : A} (f : A → B) → x ≡ y → f x ≡ f y
  congEq f refl = refl

  symEq : ∀ {ℓ} {A : Set ℓ} {x y : A} → x ≡ y → y ≡ x
  symEq refl = refl

  +-assoc : ∀ (m n k : Nat) → (m + n) + k ≡ m + (n + k)
  +-assoc zero n k = refl
  +-assoc (suc m) n k = congEq suc (+-assoc m n k)

  subst : ∀ {ℓ ℓ'} {A : Set ℓ} {P : A → Set ℓ'} {x y : A} → x ≡ y → P x → P y
  subst refl px = px

-- Arity-indexed binary trees (leaf count = Nat index).

data BinTree {ℓ : Level} (A : Set ℓ) : Nat → Set ℓ where
  leaf : A → BinTree A (suc zero)
  node : ∀ {m n} → BinTree A m → BinTree A n → BinTree A (m + n)

-- A single associator rotation at the root.
-- ((x∙y)∙z) ↦ (x∙(y∙z))

data AssocStep {ℓ : Level} {A : Set ℓ} : ∀ {n} → BinTree A n → BinTree A n → Set ℓ where
  rot : ∀ {m n k} (x : BinTree A m) (y : BinTree A n) (z : BinTree A k)
      → AssocStep
          (node (node x y) z)
          (subst {P = λ t → BinTree A t} (symEq (+-assoc m n k)) (node x (node y z)))

-- Contextual closure of AssocStep (rotate anywhere).

data AssocStepCtx {ℓ : Level} {A : Set ℓ} : ∀ {n} → BinTree A n → BinTree A n → Set ℓ where
  here  : ∀ {n} {t u : BinTree A n} → AssocStep t u → AssocStepCtx t u
  left  : ∀ {m n} {t t' : BinTree A m} {u : BinTree A n}
        → AssocStepCtx t t' → AssocStepCtx (node t u) (node t' u)
  right : ∀ {m n} {t : BinTree A m} {u u' : BinTree A n}
        → AssocStepCtx u u' → AssocStepCtx (node t u) (node t u')
