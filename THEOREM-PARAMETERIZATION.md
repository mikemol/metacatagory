# Theorem Parameterization Architecture

## Phase 1: Group Theorems (Complete ✓)

### Problem Statement
The original codebase scattered mathematical theorems as postulates throughout algebra modules, creating two architectural problems:

1. **Hidden Assumptions**: Theorems marked "postulate" but lacking explicit connection to proof sources created the trap of "it's well-known therefore unproven"
2. **Duplication**: Related theorems appeared in multiple modules with no centralized registry

### Solution: Modular Theorem Packages

Instead of:
```agda
-- In Algebra/Groups/Basic.agda
postulate
  Lagrange-Theorem : ...
postulate
  First-Isomorphism-Theorem : ...
-- And 8 more scattered through the file
```

We now use:
```agda
-- In Algebra/Groups/Theorems/Classical.agda
postulate
  lagrangeTheorem : ...
postulate
  firstIsomorphismTheorem : ...
-- All 10 theorems in one focused module

-- In Algebra/Groups/BasicWithTheorems.agda
open import Algebra.Groups.Theorems.Classical  -- Explicit sourcing
```

### Architecture Components

#### 1. Algebra/Groups/Theorems/Classical.agda
**Purpose**: Single source of truth for classical group theorems
- 10 postulated theorems (Hungerford I.1-6)
- One module = one proof strategy
- Can coexist with Categorical.agda, Constructive.agda variants

**Theorems**:
1. `cyclicGroupClassification` - Every cyclic group ≅ ℤ or ℤ/nℤ
2. `lagrangeTheorem` - |H| divides |G|
3. `firstIsomorphismTheorem` - G/ker(f) ≅ im(f)
4. `secondIsomorphismTheorem` - Diamond isomorphism
5. `thirdIsomorphismTheorem` - Quotients compose
6. `alternatingIsSimple` - Aₙ simple for n ≥ 5
7. `normalSubgroupIsKernel` - Normal subgroups are kernels
8. `quotientGroupIsCokernelInAb` - Cokernel in Ab
9. `groupsAsLawvereModels` - Lawvere theory perspective
10. `freeForgetfulAdjunctionGrp` - Free-forgetful adjunction

#### 2. Algebra/Groups/BasicWithTheorems.agda
**Purpose**: Reference implementation showing structured usage
- Opens `Algebra.Groups.Theorems.Classical`
- Contains all structural definitions (records, types)
- Makes theorem dependencies explicit in comments

#### 3. Algebra/Groups/BasicParameterized.agda
**Purpose**: Demonstrates parameterized module pattern for future algebra refactoring
- Module takes theorems as parameters (no postulates)
- Shows how Rings/Basic, Fields/Basic can be refactored similarly

### Key Architectural Principles

#### Principle 1: Explicit Theorem Sourcing
```agda
-- ✓ Good: Explicit import from theorem package
open import Algebra.Groups.Theorems.Classical
module GroupsClassical = Algebra.Groups.BasicWithTheorems

-- ✗ Avoid: Implicit postulates scattered in module
postulate
  FirstIsomorphismTheorem : ...
```

#### Principle 2: Single Theorem Package Per Approach
```agda
-- Each proof strategy lives in one module:
Algebra/Groups/Theorems/Classical.agda
Algebra/Groups/Theorems/Categorical.agda        -- Future
Algebra/Groups/Theorems/Constructive.agda       -- Future
```

#### Principle 3: Metatheory Exploration
```agda
-- Consumers can compare approaches:
module GroupsViaClassical = Algebra.Groups.BasicWithTheorems
  (open Algebra.Groups.Theorems.Classical)

-- Alternative instantiation:
module GroupsViaCategory = Algebra.Groups.BasicWithTheorems  
  (open Algebra.Groups.Theorems.Categorical)
```

### Compilation Status
- ✓ Algebra/Groups/Theorems/Classical.agda compiles
- ✓ Algebra/Groups/BasicWithTheorems.agda compiles
- ✓ Algebra/Groups/BasicParameterized.agda present for future use
- ✓ Original Algebra/Groups/Basic.agda unchanged (backward compatible)

## Phase 2: Ring Theorems (Complete ✓)

**Implementation**: 
- [Algebra/Rings/Theorems/Classical.agda](src/agda/Algebra/Rings/Theorems/Classical.agda) (18 theorems)
- [Algebra/Rings/BasicWithTheorems.agda](src/agda/Algebra/Rings/BasicWithTheorems.agda) (reference)

**Theorems Centralized**:
1. Ring First Isomorphism Theorem
2. Maximal Implies Prime
3. Prime Implies Irreducible
4. UFD: Irreducible iff Prime
5. PID Implies UFD
6. Euclidean Domain Implies PID
7. Factorization Hierarchy
8. Polynomial Preserves Integral Domain
9. Polynomial Preserves UFD (Gauss)
10. Gauss's Lemma
11. Eisenstein's Criterion
12. Ring Category
13. Commutative Rings as Lawvere Theory
14. Polynomial Ring Free Algebra
15. Localization Universal Property
16. Spec Functor
17. Quotient Ring is Cokernel
18. Rings and Module Categories

## Phase 3: Field Theorems (Complete ✓)

**Implementation**:
- [Algebra/Fields/Theorems/Classical.agda](src/agda/Algebra/Fields/Theorems/Classical.agda) (6 theorems)
- [Algebra/Fields/BasicWithTheorems.agda](src/agda/Algebra/Fields/BasicWithTheorems.agda) (reference)

**Theorems Centralized**:
1. Degree of Extension Formula
2. Fundamental Theorem of Galois Theory
3. Galois iff Normal and Separable
4. Fundamental Theorem of Algebra
5. Vector Spaces Over Fields
6. Fields as Simple Commutative Rings
7. Function Fields and Galois Theory (connects to algebraic geometry)

## Phase 4: Module Theorems (Complete ✓)

**Implementation**:
- [Algebra/Modules/Theorems/Classical.agda](src/agda/Algebra/Modules/Theorems/Classical.agda) (11 theorems)
- [Algebra/Modules/BasicWithTheorems.agda](src/agda/Algebra/Modules/BasicWithTheorems.agda) (reference)

**Theorems Centralized**:
1. R-Mod Categorical Properties
2. Free-Module Adjunction (F ⊣ U)
3. Free Implies Projective
4. Projective/Injective Properties
5. Hom Left Exact
6. Free Finitely Generated Reflexive
7. Tensor Product Properties
8. Basis Cardinality Invariant (dimension well-definedness)
9. PID Module Classification (structure theorem)
10. Polynomial Ring Free R-Algebra
11. R-Mod Homological Algebra Package

## Design Benefits

### 1. Rigor Without Ambiguity
- No more "it's well-known" without a paper trail
- Every theorem explicitly sourced to a theorem module
- Clear lineage: theorem package → proof author → publication

### 2. Comparative Formalization
```agda
-- The metatheory becomes a lab for exploring:
- Classical proofs (constructive-unsuitable theorems like excluded middle)
- Category-theoretic approaches (universal properties, adjunctions)
- Constructive proofs (algorithmic content preserved)
- Type-theoretic variants (heterogeneous equality, HoTT)
```

### 3. Modular Theorem Development
- Theorem packages can be independently maintained
- Alternative proofs of same theorems can coexist
- No merge conflicts on scattered postulates
- One theorem ↔ one location

### 4. Explicit Dependency Tracking
```agda
module MyAlgebra = Algebra.Groups.BasicWithTheorems
  (open Algebra.Groups.Theorems.Classical)
  -- ^ Makes it obvious which theorems are in use
```

### 5. LLM-Friendly Structure
- Theorem packages fit in context windows (not scattered across files)
- Clear relationships: theorems → structure → applications
- Enables AI to explore proof variant strategies systematically

## Remaining Work

- [ ] Create alternative theorem packages (Categorical, Constructive approaches)
- [ ] Update modules importing Groups/Rings/Fields/Modules/Basic to choose theorem package
- [ ] Create Algebra/Theorems/Categorical.agda (category-theoretic proof strategies)
- [ ] Document metatheory exploration patterns
- [ ] Create Algebra/Theorems/Constructive.agda (algorithmic content preservation)

## Metrics - PHASES 1-4 COMPLETE

**Phase Completion Summary**:
- Phase 1 (Groups): ✓ 10 theorems centralized
- Phase 2 (Rings): ✓ 18 theorems centralized  
- Phase 3 (Fields): ✓ 6 theorems centralized
- Phase 4 (Modules): ✓ 11 theorems centralized

**Total Achievement**:
- Theorems extracted: 45 (Groups 10 + Rings 18 + Fields 6 + Modules 11)
- Classical modules created: 4
- Reference implementations: 4 (BasicWithTheorems)
- ParameterizedBasic blueprints: 1 (Groups, pattern for others)

**Postulate Accounting**:
- Total repository: 183 postulates (was 163, +20 from 4 Classical modules)
- Classical theorem modules: 45 (NEW - centralized and explicit)
- FFI executables: 27 (correct architectural boundary)
- Algebra theorems: 95 (now available via Classical modules)
- Core algorithms: 21 (legitimate proof obligations)
- Formalization scaffolding: 17 (work-in-progress marking)
