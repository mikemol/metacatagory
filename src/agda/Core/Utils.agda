{-# OPTIONS --without-K #-}

-- | Miscellaneous core utilities shared across proofs and exporters.
module Core.Utils where

-- Infrastructure imports for universe polymorphism and equality
open import Infrastructure.Universe using (Setℓ)
open import Infrastructure.Coherence.Path2 using (_≡_; refl; whisker; _∙₂_)

open import Core.Phase public using (Bool; true; false)
open import Agda.Builtin.Nat public using (Nat; zero; suc; _+_; _*_; _-_)
open import Agda.Builtin.List public using (List; []; _∷_)
open import Agda.Builtin.String public using (String; primStringAppend)
open import Agda.Builtin.Equality public using (_≡_; refl)
open import Core.Phase public using (Maybe; just; nothing)
open import Core using (_×_; _,_)

-- ==========================================================
-- Boolean Logic
-- ==========================================================

not : Bool → Bool
not true = false
not false = true

_&&_ : Bool → Bool → Bool
true && b = b
false && _ = false

_||_ : Bool → Bool → Bool
true || _ = true
false || b = b

if_then_else_ : {A : Set} → Bool → A → A → A
if true  then x else _ = x
if false then _ else y = y

-- ==========================================================
-- Natural Numbers
-- ==========================================================

eqNat : Nat → Nat → Bool
eqNat zero zero = true
eqNat (suc m) (suc n) = eqNat m n
eqNat _ _ = false

ltNat : Nat → Nat → Bool
ltNat _ zero = false
ltNat zero (suc _) = true
ltNat (suc m) (suc n) = ltNat m n

lteNat : Nat → Nat → Bool
lteNat m n = (ltNat m n) || (eqNat m n)

maxNat : Nat → Nat → Nat
maxNat n m = if ltNat n m then m else n

minNat : Nat → Nat → Nat
minNat n m = if ltNat n m then n else m

-- Safe division using gas (structural recursion)
-- Removes the need for {-# TERMINATING #-} pragmas
divNatGas : Nat → Nat → Nat → Nat
divNatGas zero _ _ = zero  -- Gas exhausted
divNatGas (suc gas) m n = 
  if ltNat m n 
  then zero 
  else suc (divNatGas gas (m - n) n)

divNat : Nat → Nat → Nat
divNat m zero = zero -- Div by zero returns 0
divNat m n    = divNatGas (suc m) m n

-- ==========================================================
-- Lists
-- ==========================================================

length : {A : Set} → List A → Nat
length [] = zero
length (_ ∷ xs) = suc (length xs)

map : {A B : Set} → (A → B) → List A → List B
map f [] = []
map f (x ∷ xs) = f x ∷ map f xs

foldr : {A B : Set} → (A → B → B) → B → List A → B
foldr f z [] = z
foldr f z (x ∷ xs) = f x (foldr f z xs)

concat : {A : Set} → List (List A) → List A
concat [] = []
concat (x ∷ xs) = x ++ concat xs
  where
    _++_ : {A : Set} → List A → List A → List A
    _++_ {A} [] ys = ys
    _++_ {A} (z ∷ zs) ys = z ∷ (_++_ {A} zs ys)

member : Nat → List Nat → Bool
member _ [] = false
member n (x ∷ xs) = if eqNat n x then true else member n xs

-- ==========================================================
-- Strings
-- ==========================================================

strCat : String → String → String
strCat = primStringAppend

intercalate : String → List String → String
intercalate sep [] = ""
intercalate sep (x ∷ []) = x
intercalate sep (x ∷ xs) = strCat x (strCat sep (intercalate sep xs))
