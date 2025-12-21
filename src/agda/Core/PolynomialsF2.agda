{-# OPTIONS --without-K #-}

-- Core.PolynomialsF2: Minimal stdlib-free F2 polynomials and division

module Core.PolynomialsF2 where

open import Agda.Builtin.List using (List; []; _∷_)
open import Core.Phase using (Bool; true; false)
open import Agda.Builtin.Nat using (Nat; zero; suc)
open import Agda.Primitive using (Level; lzero)

-- Polynomial over F2 represented as little-endian list of bits (lowest degree first)
-- Example: [true , false , true] represents 1 + x^2
PolyF2 : Set
PolyF2 = List Bool

-- xor for Bool

_xor_ : Bool → Bool → Bool
false xor b = b
true  xor false = true
true  xor true  = false

-- and for Bool

_and_ : Bool → Bool → Bool
false and _ = false
true  and b = b

-- Check if polynomial is zero (all coefficients false)

polyZero? : PolyF2 → Bool
polyZero? [] = true
polyZero? (false ∷ xs) = polyZero? xs
polyZero? (true ∷ xs) = false

-- Normalize by trimming trailing false coefficients
normalize : PolyF2 → PolyF2
normalize [] = []
normalize (true ∷ cs) with normalize cs
... | []  = true ∷ []
... | cs' = true ∷ cs'
normalize (false ∷ cs) with normalize cs
... | []  = []
... | cs' = false ∷ cs'

-- Degree (0 for constant non-zero; -∞ not represented, treat [] as degree 0)
-- Remove older degree; use degree2 below

-- Addition is coefficient-wise xor
_⊕_ : PolyF2 → PolyF2 → PolyF2
[] ⊕ ys = ys
xs ⊕ [] = xs
(c ∷ xs) ⊕ (d ∷ ys) = (c xor d) ∷ (xs ⊕ ys)

-- Multiply by x^k (left shift by k, i.e., prepend k falses)
shift : Nat → PolyF2 → PolyF2
shift zero p = p
shift (suc k) p = false ∷ shift k p

-- Long division over F2 producing quotient and remainder
record DivResultF2 : Set where
  field
    quotient : PolyF2
    remainder : PolyF2

open DivResultF2 public

-- Helper to set a coefficient at position k in quotient
setCoeffAt : Nat → PolyF2 → PolyF2
setCoeffAt zero qs = true ∷ qs
setCoeffAt (suc k) [] = false ∷ setCoeffAt k []
setCoeffAt (suc k) (q ∷ qs) = q ∷ setCoeffAt k qs

-- Remove older longDivideF2; use longDivideF2' below

-- Boolean comparison and subtraction on Nat (minimal helpers)
_<_ : Nat → Nat → Bool
zero < zero = false
zero < suc n = true
suc m < zero = false
suc m < suc n = m < n

_−_ : Nat → Nat → Nat
m − zero = m
zero − suc n = zero
suc m − suc n = m − n

-- Redefine degree using a simpler fold with trailing trim and length-1
length : PolyF2 → Nat
length [] = zero
length (_ ∷ xs) = suc (length xs)

-- Override degree to avoid dependency on normalize rec
degree2 : PolyF2 → Nat
degree2 p with normalize p
... | [] = zero
... | (_ ∷ xs) = (length xs)

-- Use simpler degree
{-# TERMINATING #-}
longDivideF2' : PolyF2 → PolyF2 → DivResultF2
longDivideF2' dividend divisor with normalize divisor
... | [] = record { quotient = []; remainder = dividend }
... | dN = go [] (normalize dividend)
  where
    go : PolyF2 → PolyF2 → DivResultF2
    go q r with normalize r
    ... | [] = record { quotient = q ; remainder = [] }
    ... | rN with degree2 rN < degree2 dN
    ... | true = record { quotient = q ; remainder = rN }
    ... | false =
      let k = (degree2 rN) − (degree2 dN) in
      let r' = normalize (rN ⊕ shift k dN) in
      let q' = setCoeffAt k q in
      go q' r'

-- Public division: prefer the simplified version
divideF2 : PolyF2 → PolyF2 → DivResultF2
divideF2 = longDivideF2'

-- Convenience: zero polynomial check for remainder
remainderZero? : DivResultF2 → Bool
remainderZero? dr = polyZero? (DivResultF2.remainder dr)
