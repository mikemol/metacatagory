``` Agda
-- Algebra.Groups.Structure: Structure theory of groups (Hungerford Ch II)
-- This module covers finitely generated abelian groups, Sylow theorems, and group actions.

module Algebra.Groups.Structure where

open import Core
open import Chapter1.Level1Index
open import Chapter2.Level2sub3  -- Lawvere theories
open import Algebra.Foundation
open import Algebra.Groups.Basic
open import Algebra.Groups.Free
open import PropertyRegistry
open import Metamodel as M

-- ============================================================================
-- II.1: Free Abelian Groups (covered in Free.agda and Groups.Abelian)
-- ============================================================================

-- Re-export key concepts for convenience
open import Algebra.Groups.Abelian using (FreeAbelianGroup)

-- Import from Free.agda
open Algebra.Groups.Free using (FinitelyGeneratedAbelianGroup)

-- ============================================================================
-- II.2: Finitely Generated Abelian Groups
-- ============================================================================

-- Fundamental Theorem: Structure classification
-- A finitely generated abelian group is isomorphic to:
-- ℤʳ ⊕ ℤ/n₁ℤ ⊕ ... ⊕ ℤ/nₖℤ where n₁|n₂|...|nₖ (invariant factors)
-- or equivalently: ℤʳ ⊕ ℤ/p₁^e₁ℤ ⊕ ... ⊕ ℤ/pₘ^eₘℤ (elementary divisors)
record InvariantFactorDecomposition (A : FinitelyGeneratedAbelianGroup) : Set₁ where
  field
    freeRank : M.Identifier  -- r (rank of free part)
    invariantFactors : M.Identifier  -- n₁|n₂|...|nₖ
    -- A ≅ ℤʳ ⊕ ℤ/n₁ℤ ⊕ ... ⊕ ℤ/nₖℤ
    isomorphism : M.Identifier

-- Elementary divisor form (prime power decomposition)
record ElementaryDivisorDecomposition (A : FinitelyGeneratedAbelianGroup) : Set₁ where
  field
    freeRank : M.Identifier  -- r
    primePowerFactors : M.Identifier  -- p₁^e₁, ..., pₘ^eₘ
    -- A ≅ ℤʳ ⊕ ℤ/p₁^e₁ℤ ⊕ ... ⊕ ℤ/pₘ^eₘℤ
    isomorphism : M.Identifier

-- Classification theorem
postulate
  Fundamental-Theorem-Finitely-Generated-Abelian :
    (A : FinitelyGeneratedAbelianGroup) →
    M.Identifier  -- A has unique invariant factor decomposition

-- Uniqueness of decomposition
postulate
  Invariant-Factors-Unique :
    (A : FinitelyGeneratedAbelianGroup) →
    (D₁ D₂ : InvariantFactorDecomposition A) →
    M.Identifier  -- D₁ = D₂ (invariant factors are unique)

-- Torsion subgroup
record TorsionSubgroup (A : AbelianGroupDeclaration) : Set₁ where
  field
    abelianGroup : AbelianGroupDeclaration
    -- T(A) = {a ∈ A | ∃n > 0, na = 0}
    torsionElements : M.Identifier
    isSubgroup : Subgroup (AbelianGroupDeclaration.underlyingGroup abelianGroup)

-- For finitely generated A, A/T(A) is free abelian
postulate
  Quotient-By-Torsion-Is-Free :
    (A : FinitelyGeneratedAbelianGroup) →
    M.Identifier  -- A/T(A) ≅ ℤʳ for some r

-- ============================================================================
-- II.3: Krull-Schmidt Theorem
-- ============================================================================

-- Indecomposable group (categorical: no nontrivial product decomposition)
record IndecomposableGroup (G : GroupDeclaration) : Set₁ where
  field
    group : GroupDeclaration
    -- G ≇ H × K unless H or K is trivial
    isIndecomposable : M.Identifier

-- Krull-Schmidt: Unique decomposition into indecomposables
-- For groups satisfying ascending/descending chain conditions
postulate
  Krull-Schmidt-Theorem :
    (G : GroupDeclaration) →
    M.Identifier  -- G ≅ G₁ × ... × Gₙ where Gᵢ indecomposable (unique up to order)

-- This connects to categorical aspects: products as limits
postulate
  Krull-Schmidt-Categorical :
    M.Identifier  -- Decomposition reflects categorical product structure

-- ============================================================================
-- II.4: Group Actions
-- ============================================================================

-- Group action on a set (connects to representation theory)
record GroupAction (G : GroupDeclaration) (X : M.Identifier) : Set₁ where
  field
    group : GroupDeclaration
    set : M.Identifier  -- Set X
    -- Action: G × X → X
    action : M.Identifier
    -- Axioms: e·x = x, (gh)·x = g·(h·x)
    identityAction : M.Identifier
    compatibilityAction : M.Identifier

-- Orbit of an element
record Orbit (G : GroupDeclaration) (X : M.Identifier) (act : GroupAction G X) (x : M.Identifier) : Set₁ where
  field
    groupAction : GroupAction G X
    element : M.Identifier
    -- Orb(x) = {g·x | g ∈ G}
    orbitSet : M.Identifier

-- Stabilizer of an element (subgroup)
record Stabilizer (G : GroupDeclaration) (X : M.Identifier) (act : GroupAction G X) (x : M.Identifier) : Set₁ where
  field
    groupAction : GroupAction G X
    element : M.Identifier
    -- Stab(x) = {g ∈ G | g·x = x}
    stabilizer : Subgroup (GroupAction.group groupAction)

-- Orbit-Stabilizer Theorem: |Orb(x)| · |Stab(x)| = |G|
postulate
  Orbit-Stabilizer-Theorem :
    (G : GroupDeclaration) →
    (X : M.Identifier) →
    (act : GroupAction G X) →
    (x : M.Identifier) →
    M.Identifier  -- |Orb(x)| · |Stab(x)| = |G|

-- Cayley's Theorem: Every group embeds in a symmetric group
postulate
  Cayley-Theorem :
    (G : GroupDeclaration) →
    M.Identifier  -- G ↪ Sym(G) via left multiplication action

-- Categorical perspective: Group actions as functors
-- Action G × X → X corresponds to functor BG → Set
-- where BG is the one-object category (delooping of G)
record GroupActionAsFunctor (G : GroupDeclaration) : Set₁ where
  field
    group : GroupDeclaration
    -- BG is category with one object, morphisms = elements of G
    delooping : M.Identifier
    -- Functor BG → Set picks out a G-set
    toSet : M.Identifier

-- ============================================================================
-- II.5: Sylow Theorems
-- ============================================================================

-- p-group: every element has order a power of p
record PGroup (p : M.Identifier) (G : GroupDeclaration) : Set₁ where
  field
    prime : M.Identifier  -- p
    group : GroupDeclaration
    -- |G| = pⁿ for some n
    isPGroup : M.Identifier

-- Sylow p-subgroup: maximal p-subgroup
record SylowPSubgroup (p : M.Identifier) (G : GroupDeclaration) : Set₁ where
  field
    prime : M.Identifier
    group : GroupDeclaration
    -- H ⊆ G with |H| = pⁿ where pⁿ || |G|
    subgroup : Subgroup group
    subgroupAsGroup : GroupDeclaration
    isPGroup : PGroup prime subgroupAsGroup
    isMaximal : M.Identifier

-- First Sylow Theorem: Sylow p-subgroups exist
postulate
  Sylow-First :
    (G : GroupDeclaration) →
    (p : M.Identifier) →  -- prime
    M.Identifier  -- Sylow p-subgroup exists

-- Second Sylow Theorem: All Sylow p-subgroups are conjugate
postulate
  Sylow-Second :
    (G : GroupDeclaration) →
    (p : M.Identifier) →
    (P₁ P₂ : SylowPSubgroup p G) →
    M.Identifier  -- ∃ g ∈ G, P₂ = gP₁g⁻¹

-- Third Sylow Theorem: Number of Sylow p-subgroups
postulate
  Sylow-Third :
    (G : GroupDeclaration) →
    (p : M.Identifier) →
    M.Identifier  -- nₚ ≡ 1 (mod p) and nₚ | |G|

-- Sylow theorems via group actions
-- The proof uses G acting on Sylow p-subgroups by conjugation
postulate
  Sylow-Via-Group-Action :
    M.Identifier  -- Conjugation action gives Sylow theorems

-- ============================================================================
-- II.6: Classification of Finite Groups
-- ============================================================================

-- Simple group (no proper normal subgroups)
record SimpleGroup (G : GroupDeclaration) : Set₁ where
  field
    group : GroupDeclaration
    -- No normal subgroups except {e} and G
    isSimple : M.Identifier

-- Classification of finite simple groups (CFSG)
-- This is the massive theorem completed in 2004
postulate
  Classification-Finite-Simple-Groups :
    M.Identifier  -- Complete list: cyclic, alternating, Lie type, sporadic

-- Composition series
record CompositionSeries (G : GroupDeclaration) : Set₁ where
  field
    group : GroupDeclaration
    -- {e} = G₀ ⊴ G₁ ⊴ ... ⊴ Gₙ = G where Gᵢ/Gᵢ₋₁ simple
    series : M.Identifier
    factorsAreSimple : M.Identifier

-- Jordan-Hölder Theorem: Composition factors are unique
postulate
  Jordan-Holder-Theorem :
    (G : GroupDeclaration) →
    (S₁ S₂ : CompositionSeries G) →
    M.Identifier  -- Same composition factors (up to permutation)

-- ============================================================================
-- II.7: Nilpotent and Solvable Groups
-- ============================================================================

-- Commutator subgroup [G,G]
record CommutatorSubgroup (G : GroupDeclaration) : Set₁ where
  field
    group : GroupDeclaration
    -- [G,G] = ⟨[g,h] | g,h ∈ G⟩ where [g,h] = ghg⁻¹h⁻¹
    commutatorSubgroup : NormalSubgroup group

-- Derived series: G⁽⁰⁾ = G, G⁽ⁿ⁺¹⁾ = [G⁽ⁿ⁾, G⁽ⁿ⁾]
record DerivedSeries (G : GroupDeclaration) : Set₁ where
  field
    group : GroupDeclaration
    -- G ⊇ [G,G] ⊇ [[G,G],[G,G]] ⊇ ...
    series : M.Identifier

-- Solvable group: derived series reaches {e}
record SolvableGroup (G : GroupDeclaration) : Set₁ where
  field
    group : GroupDeclaration
    derivedSeries : DerivedSeries group
    -- G⁽ⁿ⁾ = {e} for some n
    isSolvable : M.Identifier

-- Lower central series: G₀ = G, Gₙ₊₁ = [G, Gₙ]
record LowerCentralSeries (G : GroupDeclaration) : Set₁ where
  field
    group : GroupDeclaration
    series : M.Identifier

-- Nilpotent group: lower central series reaches {e}
record NilpotentGroup (G : GroupDeclaration) : Set₁ where
  field
    group : GroupDeclaration
    lowerCentralSeries : LowerCentralSeries group
    isNilpotent : M.Identifier

-- Nilpotent implies solvable
postulate
  Nilpotent-Implies-Solvable :
    (G : GroupDeclaration) →
    (N : NilpotentGroup G) →
    M.Identifier  -- G is solvable

-- Finite p-groups are nilpotent
postulate
  PGroups-Are-Nilpotent :
    (p : M.Identifier) →
    (G : GroupDeclaration) →
    (P : PGroup p G) →
    M.Identifier  -- G is nilpotent

-- Solvability via abelian quotients (categorical perspective)
postulate
  Solvable-Via-Abelian-Quotients :
    (G : GroupDeclaration) →
    M.Identifier  -- G solvable iff composition series has abelian factors

-- ============================================================================
-- II.8: Normal and Subnormal Series
-- ============================================================================

-- Normal series
record NormalSeries (G : GroupDeclaration) : Set₁ where
  field
    group : GroupDeclaration
    -- {e} = G₀ ⊴ G₁ ⊴ ... ⊴ Gₙ = G
    series : M.Identifier
    eachIsNormal : M.Identifier

-- Subnormal series (weaker: only Gᵢ ⊴ Gᵢ₊₁, not necessarily Gᵢ ⊴ G)
record SubnormalSeries (G : GroupDeclaration) : Set₁ where
  field
    group : GroupDeclaration
    series : M.Identifier
    successiveNormality : M.Identifier

-- Refinement of series
record SeriesRefinement (G : GroupDeclaration) (S : NormalSeries G) : Set₁ where
  field
    originalSeries : NormalSeries G
    refinedSeries : NormalSeries (NormalSeries.group originalSeries)
    isRefinement : M.Identifier

-- Schreier Refinement Theorem
postulate
  Schreier-Refinement-Theorem :
    (G : GroupDeclaration) →
    (S₁ S₂ : NormalSeries G) →
    M.Identifier  -- S₁ and S₂ have equivalent refinements

-- ============================================================================
-- Integration with Category Theory and Lawvere Theories
-- ============================================================================

-- Finitely generated abelian groups as models of Lawvere theory
postulate
  FG-Abelian-As-Lawvere-Models :
    M.Identifier  -- FGAb ≃ certain subcategory of Mod(Th(Ab), Set)

-- Group actions correspond to functors from delooping
postulate
  Group-Action-Functor-Correspondence :
    (G : GroupDeclaration) →
    M.Identifier  -- G-sets ≃ Functors(BG, Set)

-- Sylow subgroups and categorical quotients
postulate
  Sylow-Categorical-Perspective :
    M.Identifier  -- Conjugation classes relate to categorical orbit decomposition

-- Composition series and filtrations in abelian categories
-- This prepares for Module theory (Ch IV) and exact sequences
postulate
  Composition-Series-As-Filtration :
    (G : GroupDeclaration) →
    M.Identifier  -- When abelian, series is filtered object in Ab

-- Solvable groups and iterated extensions
postulate
  Solvable-As-Iterated-Extension :
    (G : GroupDeclaration) →
    M.Identifier  -- G solvable iff built from abelian groups by extensions

-- Nilpotent groups and central extensions
postulate
  Nilpotent-As-Central-Extension :
    (G : GroupDeclaration) →
    M.Identifier  -- G nilpotent iff built by iterated central extensions

-- Connection to homological algebra (prepares for Ch IV)
postulate
  Group-Extensions-And-Cohomology :
    M.Identifier  -- Extensions classified by H²(G, A)
```
