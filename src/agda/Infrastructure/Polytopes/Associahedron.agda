-- | Associahedron paths and composition helpers.
module Infrastructure.Polytopes.Associahedron where

open import Agda.Primitive using (Level)
open import Agda.Builtin.Nat using (Nat)
open import Agda.Builtin.Equality using (_≡_; refl)

private
  congPath : ∀ {ℓ ℓ'} {A : Set ℓ} {B : Set ℓ'} {x y : A} (f : A → B) → x ≡ y → f x ≡ f y
  congPath f refl = refl

open import Infrastructure.Arity.BinTree using (BinTree; AssocStepCtx)

-- Path in the (contextual) associator-rotation graph.

data Path {ℓ : Level} {A : Set ℓ} {n : Nat} : BinTree A n → BinTree A n → Set ℓ where
  []  : ∀ {t} → Path t t
  _∷_ : ∀ {t u v} → AssocStepCtx {A = A} {n = n} t u → Path u v → Path t v

infixr 5 _∷_

_++_ : ∀ {ℓ : Level} {A : Set ℓ} {n : Nat} {x y z : BinTree A n} → Path x y → Path y z → Path x z
[]        ++ q = q
(s ∷ p)   ++ q = s ∷ (p ++ q)

-- Associativity of path concatenation: the key "triangle identity" for our path calculus.
++-assoc : ∀ {ℓ : Level} {A : Set ℓ} {n : Nat} {w x y z : BinTree A n}
         → (p : Path w x) (q : Path x y) (r : Path y z)
         → (p ++ q) ++ r ≡ p ++ (q ++ r)
++-assoc [] q r = refl
++-assoc (s ∷ p) q r = congPath (λ t → s ∷ t) (++-assoc p q r)
