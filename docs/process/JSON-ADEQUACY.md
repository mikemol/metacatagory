# Adequacy Framework for JSON Transformations

**Date:** 2026-01-04  
**Provenance:** Phase 1 adequacy architecture review  
**Related:** [JSON-DECOMPOSITION.md](JSON-DECOMPOSITION.md), [JSONTransformation.agda](../../src/agda/Plan/CIM/JSONTransformation.agda), [JSONTransformationAdequacy.agda](../../src/agda/Plan/CIM/JSONTransformationAdequacy.agda)

## Executive Summary

Instead of implementing JSON transformations via high-level Python FFI (calling external scripts from Agda), we leverage the project's **adequacy framework** to synthesize transformations from compositional primitives. This approach:

1. **Eliminates FFI complexity** - no Python interop layer needed
2. **Provides formal guarantees** - adequacy proofs witness that primitives suffice
3. **Enables extraction** - verified Agda code extracts to Haskell (optionally Python)
4. **Follows established patterns** - reuses FunctionCategory, TransformationSystem adequacy architecture

## The Adequacy Pattern

### Core Concept

> **Adequacy Invariant**: "An axiom is a parameter whose associated theorems must be constructible from its generators."

Instead of postulating entire functions (e.g., `forward : Monolithic → Hierarchical`), we:

1. Define **primitive operations** (small, verifiable building blocks)
2. Package primitives into an **adequacy kit** (concrete instance data)
3. **Synthesize** complex operations from kit primitives
4. **Prove** adequacy: kit suffices to solve the associated "face" (proof obligation)

### Example: FunctionCategory

```agda
-- Traditional approach (postulated):
postulate
  forward : Monolithic → Hierarchical

-- Adequacy approach:
record FunctionKit : Set where
  field
    A B : Set
    f   : A → B  -- primitive

-- Synthesized operation:
-- Function composition, identity, etc. built from kit's 'f'

-- Adequacy proof:
-- Kit.f suffices to discharge faces in the function path algebra
```

## JSON Transformation Adequacy

### Primitive Operations (Generators)

Instead of postulating `forward` and `backward`, we postulate **small, atomic operations**:

```agda
-- JSON navigation
postulate
  json-get  : JSON → String → Maybe JSON
  json-at   : JSON → Nat → Maybe JSON
  json-keys : JSON → List String

-- JSON construction  
postulate
  json-empty : JSON
  json-set   : JSON → String → JSON → JSON
  json-merge : JSON → JSON → JSON

-- JSON I/O
postulate
  json-serialize : JSON → String
  json-parse     : String → Maybe JSON
```

**Why this is better:**
- Each primitive is **simple** (trivial to implement/verify)
- Each primitive is **reusable** across many transformations
- Primitives compose **predictably** (functional, no side effects)

### Adequacy Kit

Package the primitives with a concrete transformation strategy:

```agda
record JSONTransformationKit : Set where
  field
    monolithic  : Monolithic              -- Input data
    targetRoot  : Filepath                -- Where to write hierarchy
    strategy    : JSON → List (Filepath × JSON)  -- How to decompose
    metadataGen : JSON → JSON             -- Stats extraction
    indexGen    : List Fragment → JSON    -- Index manifest generation
    manifest    : ManifestSpec            -- Reconstruction rules
```

**Key insight:** The kit doesn't DO the transformation—it provides the **ingredients**. The transformation is **synthesized** from these ingredients.

### Synthesized Operations

Build complex operations by composing kit primitives:

```agda
-- Extract metadata field from monolithic JSON
extractMetadata : JSON → JSON
extractMetadata j = 
  case json-get j "metadata" of λ where
    (just m) → m
    nothing  → json-empty

-- Build hierarchical structure from kit
buildHierarchical : JSONTransformationKit → Hierarchical
buildHierarchical kit = 
  let content   = Monolithic.content (kit.monolithic)
      fragments = map (λ (path , json) → mkFragment path json) 
                      (kit.strategy content)
      metadata  = kit.metadataGen content
      idx       = kit.indexGen fragments
  in mkHierarchical metadata fragments kit.manifest
```

### Path Algebra

Model transformations as paths in a state space:

```agda
data JSONState : Set where
  mono : Monolithic → JSONState
  hier : Hierarchical → JSONState

data JSONPath : JSONState → JSONState → Set where
  id-path        : ∀ {s} → JSONPath s s
  decompose-step : ∀ {m} → Strategy → JSONPath (mono m) (hier ...)
  recompose-step : ∀ {h} → Rules    → JSONPath (hier h) (mono ...)
  _⊙_           : ∀ {s₁ s₂ s₃} → JSONPath s₁ s₂ → JSONPath s₂ s₃ → JSONPath s₁ s₃
```

**Roundtrip property:**
```agda
-- The face we need to solve:
roundtrip-face : Kit → Face
roundtrip-face kit = 
  { lhs = decompose-step (kit.strategy) ⊙ recompose-step (kit.manifest)
  , rhs = id-path
  }

-- Adequacy proof obligation:
adequacy : ∀ (kit : JSONTransformationKit) → 
  solve (roundtrip-face kit) kit ≡ refl
```

## Concrete Kits

### Dependency Graph Kit

```agda
dependencyGraphKit : JSONTransformationKit
dependencyGraphKit = record
  { monolithic  = mkMonolithic (read "data/dependency_graph.json")
  ; targetRoot  = "data/deps/"
  ; strategy    = λ json → 
      -- Extract nodes array, map each to modules/{Package}/{Module}.json
      let nodes = json-array-items (json-get json "nodes")
      in map (λ node → 
        let mod = json-get node "module"
            path = "modules/" ++ moduleToPath mod ++ ".json"
        in (path , node)
      ) nodes
  ; metadataGen = λ json →
      json-set (json-set json-empty "total_modules" 
        (json-get json "metadata" >>= json-get "total_modules"))
        "source_file" "data/dependency_graph.json"
  ; indexGen = generateModuleIndex  -- defined in terms of primitives
  ; manifest = mkManifest "data/deps/_metadata.json" 
                          ["data/deps/_index.json"] 
                          "merge-modules-by-manifest"
  }
```

### Benefits vs. Python FFI

| Aspect | Python FFI | Adequacy Kit |
|--------|-----------|--------------|
| **Verification** | External, untrusted | Proven adequate in Agda |
| **Reusability** | Monolithic scripts | Compositional primitives |
| **Extraction** | Manual sync | Automatic (Agda → Haskell) |
| **Debugging** | Print statements | Type-driven, holes |
| **Dependencies** | Python runtime | Pure Agda (or extracted Haskell) |
| **Proof burden** | Trust external code | Prove kit sufficiency |

## Implementation Strategy

### Phase 1 (Current): Formalization ✅

- [x] Define `JSONTransformation.agda` (types, postulates)
- [x] Define `JSONTransformationAdequacy.agda` (kit, path algebra, adequacy instance)
- [x] Update roadmap to reflect adequacy approach

### Phase 2: Primitive Implementation

**Option A: Agda implementation (pure, extractable)**
```agda
-- Implement primitives in Agda using String operations
json-get : JSON → String → Maybe JSON
json-get j key = ... -- parse, navigate, extract

-- Extract to Haskell via MAlonzo
-- Compile: agda -c JSONTransformationAdequacy.agda
```

**Option B: FFI to minimal primitives (not full transformation)**
```agda
-- FFI only for low-level operations
{-# FOREIGN GHC import Data.Aeson #-}
{-# COMPILE GHC json-get = \j k -> ... #-}

-- Transformation logic stays in Agda, verified
```

**Option C: Postulates + external validation**
```agda
-- Keep postulates, implement separately
-- Use Agda spec as contract for implementation
-- Validate with property tests
```

### Phase 3: Kit Instantiation

1. Define `dependencyGraphKit` with concrete strategy
2. Define `enrichedRoadmapKit` with annotation indexing
3. Define `planningIndexKit` with multi-view organization

### Phase 4: Adequacy Proofs

Fill the `{!!}` holes in JSONTransformationAdequacy.agda:
- Path algebra associativity
- Identity laws
- Roundtrip witnesses
- Structure preservation

## Comparison to Existing Adequacy Instances

### FunctionCategory
- **Primitives:** Single function `f : A → B`
- **Kit:** Wraps function with domain/codomain
- **Adequacy:** Trivial (function solves its own face)

### TransformationSystem
- **Primitives:** Transformation steps, path composition
- **Kit:** Sequence of steps + start/end points
- **Adequacy:** Prove path exists from start to end

### JSON Transformation
- **Primitives:** JSON navigation, construction, merging
- **Kit:** Decomposition strategy + reconstruction rules
- **Adequacy:** Prove roundtrip = identity

**Pattern consistency:**
All three follow: `Kit + Primitives → Synthesized Operation + Proof Obligation`

## Architectural Alignment

### SPPF Model
- **Fragments** = SPPF nodes (reusable subtrees)
- **Index manifests** = SPPF navigation structure
- **Kit strategy** = SPPF traversal/construction algorithm

### Category Theory
- **Natural transformation** η : Monolithic ⇒ Hierarchical
- **Adequacy kit** = η's naturality witnesses
- **Primitives** = category morphisms (compose, associate)

### Roadmap Integration
- BUILD-JSON-SCHEMA: Extended with adequacy formalization
- BUILD-JSON-FORWARD: Synthesize from kit (not Python script)
- BUILD-JSON-BACKWARD: Dual synthesis from same primitives
- BUILD-JSON-VALIDATION: Adequacy proof = built-in validation

## Next Steps

1. **Implement primitives** (Option A recommended: pure Agda + extraction)
2. **Instantiate dependency graph kit** (pilot application)
3. **Prove associativity/identity holes** (complete path algebra)
4. **Extract and test** (Agda → Haskell → validate on real data)
5. **Replicate for enriched/planning** (reuse primitives, new strategies)

## Traceability

- **Design:** This document + [JSON-DECOMPOSITION.md](JSON-DECOMPOSITION.md)
- **Formalization:** [JSONTransformationAdequacy.agda](../../src/agda/Plan/CIM/JSONTransformationAdequacy.agda)
- **Examples:** [Examples/FunctionCategoryAdequacy.agda](../../src/agda/Examples/FunctionCategoryAdequacy.agda)
- **Infrastructure:** [Infrastructure/Axiom/Adequacy.agda](../../src/agda/Infrastructure/Axiom/Adequacy.agda)
- **Roadmap:** BUILD-JSON-SCHEMA (updated with adequacy approach)

---

**Status:** Architecture documented, adequacy module created, awaiting Phase 2 (primitive implementation).

This approach exemplifies the project's core philosophy: **formal specification precedes implementation, with adequacy as the bridge between abstract theory and concrete computation.**
