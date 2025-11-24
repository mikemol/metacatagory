-- Tests.RealWorldAlgorithmsTests: Basic test harness for real-world algorithms

module Tests.RealWorldAlgorithmsTests where

open import Agda.Primitive using (Level)
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.Equality using (_≡_; refl)
open import Core.Phase using (Bool; true; false)

open import Core.Phase using
  ( Phase
  ; mkPhase
  ; _$ₚ_
  ; _×_
  ; _,_
  ; fst
  ; snd
  ; ℕ; zero; suc
  )

import Examples.RealWorldAlgorithms as R

-- Re-export some numerals for brevity
one two three four five six seven eight : ℕ
one = R.one
two = R.two
three = R.three
four = R.four
five = R.five
six = R.six
seven = R.seven
eight = R.eight

-- Factorial as a phase on small inputs
fact-test-0 : R.factPhase $ₚ zero ≡ one
fact-test-0 = refl

_∗_ : ℕ → ℕ → ℕ
_∗_ = R._*_

fact-test-3 : R.factPhase $ₚ three ≡ (three ∗ (two ∗ one))
fact-test-3 = refl

-- Exponentiation via phase
pow-test-2^3 : R.powPhase $ₚ (two , three) ≡ eight
pow-test-2^3 = refl

-- Reverse via phase
rev-test : R.reversePhase {A = ℕ} $ₚ (one ∷ two ∷ three ∷ []) ≡ (three ∷ two ∷ one ∷ [])
rev-test = refl

-- Sortedness proof for sort (boolean)
sorted-true-312 : R.isSortedB (R.sort (three ∷ one ∷ two ∷ [])) ≡ true
sorted-true-312 = R.sorted-sort-true (three ∷ one ∷ two ∷ [])

-- Multiset equality strengthens correctness: sort is a permutation (on this example)
perm-312 : R.multisetEq (three ∷ one ∷ two ∷ []) (R.sort (three ∷ one ∷ two ∷ [])) ≡ true
perm-312 = refl

-- GCD tests
gcd-8-6 : R.gcdPhase $ₚ (eight , six) ≡ two
gcd-8-6 = refl

gcd-6-8 : R.gcdPhase $ₚ (six , eight) ≡ two
gcd-6-8 = refl

-- Sort tests
sort-312 : R.sortPhase $ₚ (three ∷ one ∷ two ∷ []) ≡ (one ∷ two ∷ three ∷ [])
sort-312 = refl

-- Graph and traversals (fuel, src)
dummyGraph : R.Graph
dummyGraph = R.mkGraph ((one , (two ∷ [])) ∷ (two , (three ∷ [])) ∷ (three , []) ∷ [])

dfs-visits : R.dfsPhase $ₚ (dummyGraph , (eight , one)) ≡ (one ∷ two ∷ three ∷ [])
dfs-visits = refl

bfs-visits : R.bfsPhase $ₚ (dummyGraph , (eight , one)) ≡ (one ∷ two ∷ three ∷ [])
bfs-visits = refl

-- GCD divisibility properties (boolean)
gcd-divides-left-8-6 : R.dividesB (R.gcd eight six) eight ≡ true
gcd-divides-left-8-6 = R.gcdDividesLeft eight six

gcd-divides-right-8-6 : R.dividesB (R.gcd eight six) six ≡ true
gcd-divides-right-8-6 = R.gcdDividesRight eight six
