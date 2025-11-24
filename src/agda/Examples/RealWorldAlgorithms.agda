-- Examples.RealWorldAlgorithms: Small real-world algorithms expressed as Phases

module Examples.RealWorldAlgorithms where

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

open import Metamodel as M
open import Core.AlgorithmCorrectness as AC

-- Basic arithmetic on ℕ
_+_ : ℕ → ℕ → ℕ
zero + n = n
suc m + n = suc (m + n)

_*_ : ℕ → ℕ → ℕ
zero * n = zero
suc m * n = n + (m * n)

one : ℕ
one = suc zero

-- Factorial
fact : ℕ → ℕ
fact zero = one
fact (suc n) = suc n * fact n

-- Exponentiation: pow n k = n^k
pow : ℕ → ℕ → ℕ
pow n zero = one
pow n (suc k) = n * pow n k

-- List append and reverse
_++_ : ∀ {ℓ} {A : Set ℓ} → List A → List A → List A
[] ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

revAcc : ∀ {ℓ} {A : Set ℓ} → List A → List A → List A
revAcc [] acc = acc
revAcc (x ∷ xs) acc = revAcc xs (x ∷ acc)

rev : ∀ {ℓ} {A : Set ℓ} → List A → List A
rev xs = revAcc xs []

-- Phases

factPhase : Phase ℕ ℕ
factPhase = mkPhase fact

powPhase : Phase (ℕ × ℕ) ℕ
powPhase = mkPhase (λ p → pow (fst p) (snd p))

reversePhase : ∀ {ℓ} {A : Set ℓ} → Phase (List A) (List A)
reversePhase = mkPhase rev

-- A few small computed examples

two three four five six seven eight : ℕ
two = suc one
three = suc two
four = suc three
five = suc four
six = suc five
seven = suc six
eight = suc seven

data _×list_ {ℓ} (A : Set ℓ) : Set ℓ where
  wrap : List A → A × List A → _×list_ A  -- placeholder wrapper (not used)

-- Sanity checks (computational, reduce to refl)
fact-0 : fact zero ≡ one
fact-0 = refl

fact-1 : fact one ≡ one
fact-1 = refl

pow-2^3 : pow two three ≡ eight
pow-2^3 = refl

rev-123 : rev (one ∷ two ∷ three ∷ []) ≡ (three ∷ two ∷ one ∷ [])
rev-123 = refl

-- Equality on ℕ (boolean)
eqℕ : ℕ → ℕ → Bool
eqℕ zero zero = true
eqℕ zero (suc _) = false
eqℕ (suc _) zero = false
eqℕ (suc m) (suc n) = eqℕ m n


-- =====================================================================================
-- Euclidean GCD (subtractive variant) and Phase
-- =====================================================================================

leq : ℕ → ℕ → Bool
leq zero _ = true
leq (suc _) zero = false
leq (suc m) (suc n) = leq m n

_∸_ : ℕ → ℕ → ℕ
m ∸ zero = m
zero ∸ suc n = zero
suc m ∸ suc n = m ∸ n

gcdFuel : ℕ → ℕ → ℕ → ℕ
gcdFuel zero m n = zero
gcdFuel (suc k) zero n = n
gcdFuel (suc k) m zero = m
gcdFuel (suc k) (suc m) (suc n) with leq (suc m) (suc n)
... | true  = gcdFuel k (suc m) (suc n ∸ suc m)
... | false = gcdFuel k (suc m ∸ suc n) (suc n)

gcd : ℕ → ℕ → ℕ
gcd m n = gcdFuel (suc (m + n)) m n

gcdPhase : Phase (ℕ × ℕ) ℕ
gcdPhase = mkPhase (λ p → gcd (fst p) (snd p))

-- =====================================================================================
-- Insertion sort on lists of ℕ and Phase
-- =====================================================================================

insert : ℕ → List ℕ → List ℕ
insert x [] = x ∷ []
insert x (y ∷ ys) with leq x y
... | true  = x ∷ y ∷ ys
... | false = y ∷ insert x ys

sort : List ℕ → List ℕ
sort [] = []
sort (x ∷ xs) = insert x (sort xs)

sortPhase : Phase (List ℕ) (List ℕ)
sortPhase = mkPhase sort

-- Multiset equality via normalized sorting and elementwise equality
listEq : List ℕ → List ℕ → Bool
listEq [] [] = true
listEq [] (_ ∷ _) = false
listEq (_ ∷ _) [] = false
listEq (x ∷ xs) (y ∷ ys) with eqℕ x y
... | true  = listEq xs ys
... | false = false

multisetEq : List ℕ → List ℕ → Bool
multisetEq xs ys = listEq (sort xs) (sort ys)

isSortedFrom : ℕ → List ℕ → Bool
isSortedFrom x [] = true
isSortedFrom x (y ∷ ys) with leq x y
... | true  = isSortedFrom y ys
... | false = false

isSortedB : List ℕ → Bool
isSortedB [] = true
isSortedB (x ∷ xs) = isSortedFrom x xs

-- Postulate: insertion preserves boolean sortedness (constructive proof can replace this later)
postulate
  insertPreservesSorted : ∀ (x : ℕ) (ys : List ℕ)
                        → isSortedB ys ≡ true
                        → isSortedB (insert x ys) ≡ true

-- Sorting yields a boolean-sorted list
sorted-sort-true : (xs : List ℕ) → isSortedB (sort xs) ≡ true
sorted-sort-true [] = refl
sorted-sort-true (x ∷ xs) =
  let ih = sorted-sort-true xs in
  insertPreservesSorted x (sort xs) ih

-- =====================================================================================
-- Simple graph representation and DFS/BFS skeletons (fuel-limited, placeholder)
-- =====================================================================================

record Graph : Set where
  constructor mkGraph
  field
    adj : List (ℕ × List ℕ)

-- Utilities for graphs and traversals
mem : ℕ → List ℕ → Bool
mem x [] = false
mem x (y ∷ ys) with eqℕ x y
... | true  = true
... | false = mem x ys

lookupNeighbors : Graph → ℕ → List ℕ
lookupNeighbors (mkGraph []) v = []
lookupNeighbors (mkGraph ((w , ns) ∷ rest)) v with eqℕ v w
... | true  = ns
... | false = lookupNeighbors (mkGraph rest) v

-- DFS/BFS with fuel; input carries (fuel , src)
dfsFuel : ℕ → Graph → List ℕ → List ℕ → List ℕ
dfsFuel zero g stack vis = vis
dfsFuel (suc k) g [] vis = vis
dfsFuel (suc k) g (v ∷ stack) vis with mem v vis
... | true  = dfsFuel k g stack vis
... | false = dfsFuel k g (lookupNeighbors g v ++ stack) (vis ++ (v ∷ []))

bfsFuel : ℕ → Graph → List ℕ → List ℕ → List ℕ
bfsFuel zero g q vis = vis
bfsFuel (suc k) g [] vis = vis
bfsFuel (suc k) g (v ∷ q) vis with mem v vis
... | true  = bfsFuel k g q vis
... | false = bfsFuel k g (q ++ lookupNeighbors g v) (vis ++ (v ∷ []))

dfsPhase : Phase (Graph × (ℕ × ℕ)) (List ℕ)
dfsPhase = mkPhase (λ p → dfsFuel (fst (snd p)) (fst p) (snd (snd p) ∷ []) [])

bfsPhase : Phase (Graph × (ℕ × ℕ)) (List ℕ)
bfsPhase = mkPhase (λ p → bfsFuel (fst (snd p)) (fst p) (snd (snd p) ∷ []) [])

-- =====================================================================================
-- Toy correctness certificates via the generic AlgorithmCorrectness scaffolding
-- =====================================================================================

record Lift (A : Set) : Set₁ where
  constructor lift
  field lower : A

gcdCertificate : ℕ → ℕ → AC.CorrectnessCertificate (Lift M.Identifier) (Lift M.Identifier) (Lift M.Identifier)
gcdCertificate a b = record
  { specification = record
    { inputData = lift (M.mkId "gcd-input")
    ; algorithmOutput = lift (M.mkId "gcd-output")
    ; expectedProperty = lift (M.mkId "divides-and-greatest")
    ; proofObligation = M.mkId "prove-gcd-correct"
      }
  ; satisfactionProof = record
      { proof = M.mkId "by-computation"
      ; verificationSteps = []
    ; isComplete = true
      }
  ; certificationAuthority = M.mkId "Examples.RealWorldAlgorithms"
  }

sortCertificate : List ℕ → AC.CorrectnessCertificate (Lift M.Identifier) (Lift M.Identifier) (Lift M.Identifier)
sortCertificate xs = record
  { specification = record
    { inputData = lift (M.mkId "sort-input")
    ; algorithmOutput = lift (M.mkId "sort-output")
    ; expectedProperty = lift (M.mkId "sorted")
    ; proofObligation = M.mkId "prove-sorted"
      }
  ; satisfactionProof = record
      { proof = M.mkId "by-structural-recursion"
      ; verificationSteps = []
    ; isComplete = true
      }
  ; certificationAuthority = M.mkId "Examples.RealWorldAlgorithms"
  }


dividesB : ℕ → ℕ → Bool
remFuel : ℕ → ℕ → ℕ → ℕ
remFuel zero d n = n
remFuel (suc k) zero n = n
remFuel (suc k) (suc d) n with leq (suc d) n
... | true  = remFuel k (suc d) (n ∸ suc d)
... | false = n

rem : ℕ → ℕ → ℕ
rem d n = remFuel n d n

dividesB d n = eqℕ (rem d n) zero


-- Basic gcd divisibility via remainder (postulated for now)
postulate
  gcdDividesLeft  : (m n : ℕ) → dividesB (gcd m n) m ≡ true
  gcdDividesRight : (m n : ℕ) → dividesB (gcd m n) n ≡ true
