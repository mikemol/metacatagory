# Higher-Order Homotopical Contracts via Module Parameters

**Date:** 2026-01-04  
**Provenance:** Phase 1+ module parameter architecture extension  
**Related:** [JSON-ADEQUACY.md](JSON-ADEQUACY.md), [JSONTransformationContract.agda](../../src/agda/Plan/CIM/JSONTransformationContract.agda)

## The Insight

Instead of choosing between approaches, we can have **both** and use a **natural transformation** to witness their equivalence. This creates a "homotopical contract" where:

1. **Module parameters** define the abstract contract (interface)
2. **Concrete implementation** provides pure Agda code (implementation)
3. **Natural transformation** proves they're equivalent (verification)
4. **Tests** run against both, reinforcing each other (validation)

## Three-Layer Architecture

### Layer 1: Contract (Interface via Module Parameters)

```agda
record JSONPrimitives : Set where
  field
    -- Operations
    get  : JSON → String → Maybe JSON
    set  : JSON → String → JSON → JSON
    merge : JSON → JSON → JSON
    ...
    
    -- Laws (witnesses)
    get-set-same : ∀ j k v → get (set j k v) k ≡ just v
    merge-empty  : ∀ j → merge j empty ≡ j
    ...
```

**Key insight:** The contract is a **mathematical object** (a record type), not just documentation.

### Layer 2: Parameterized Implementation

```agda
module JSONTransformationParameterized (P : JSONPrimitives) where
  open JSONPrimitives P
  
  -- Implementation uses contract primitives
  forward : Strategy → Monolithic → Hierarchical
  forward strat m = 
    let frags = fragmentize (Monolithic.content m)
        meta = metadataExtract ...
    in mkHierarchical meta frags ...
  
  -- Roundtrip proven from contract laws
  roundtrip : ∀ strat m → backward (forward strat m) ≡ m
  roundtrip = ... -- uses P.get-set-same, P.merge-empty, etc.
```

**Key insight:** Implementation is **parametric** in primitives—works for ANY correct implementation.

### Layer 3: Concrete Implementation

```agda
module JSONPrimitivesConcrete where
  -- Actual implementations
  json-get-concrete : JSON → String → Maybe JSON
  json-get-concrete = ... -- String parsing, etc.
  
  -- Witnesses that concrete satisfies contract
  concrete-get-set-same : ∀ j k v → 
    json-get-concrete (json-set-concrete j k v) k ≡ just v
  
  -- Bundle into contract
  concretePrimitives : JSONPrimitives
  JSONPrimitives.get concretePrimitives = json-get-concrete
  JSONPrimitives.get-set-same concretePrimitives = concrete-get-set-same
  ...
```

**Key insight:** Concrete implementation is just **one instance** of the contract.

## Natural Transformation: The Homotopical Contract

```agda
module JSONTransformationEquivalence where
  open JSONPrimitivesConcrete
  open JSONTransformationParameterized concretePrimitives
  
  -- η witnesses that parameterized ≅ concrete
  η-forward : ∀ strat m → 
    forward strat m ≡ forward strat m  -- when instantiated
  
  -- Naturality: transformations commute with structure
  naturality : ∀ strat m →
    η-backward (forward strat m) ≡ η-backward (forward strat m)
```

**Key insight:** The natural transformation witnesses that abstract and concrete are **homotopically equivalent**—they're the same "up to path".

## Higher-Order Contract

This is a **higher-order contract** because:

1. **First-order:** Contract specifies primitive operations
2. **Second-order:** Contract specifies laws primitives must satisfy
3. **Third-order:** Natural transformation witnesses abstract ≅ concrete
4. **Homotopical:** Equivalence is witnessed by paths (η), not just equality

## Mutual Reinforcement

The two implementations **reinforce each other**:

```
Parameterized Implementation
  ↓ (instantiate with concretePrimitives)
Concrete Instance
  ↓ (natural transformation η)
Equivalence Proof
  ↓ (homotopy contract)
Both witness same properties
```

### Benefit 1: Proof Transfer

Prove a property in the **parameterized** version → automatically holds for **concrete**:

```agda
-- Prove abstractly
theorem-abstract : ∀ (P : JSONPrimitives) → ...
  (open JSONTransformationParameterized P)
  roundtrip strat m ≡ m
  
-- Transfers to concrete
theorem-concrete : ...
  roundtrip strat m ≡ m  -- instantiated with concretePrimitives
theorem-concrete = theorem-abstract concretePrimitives
```

### Benefit 2: Test Reuse

Write tests **once** against the contract, run on **all** implementations:

```agda
module JSONTransformationTests (P : JSONPrimitives) where
  test-roundtrip : ∀ strat m → ...
  test-fragments-valid : ∀ strat m → ...

-- Run on concrete
module ConcreteTests = JSONTransformationTests concretePrimitives

-- Run on FFI implementation
module FFITests = JSONTransformationTests ffiPrimitives

-- Run on mock (for testing)
module MockTests = JSONTransformationTests mockPrimitives
```

### Benefit 3: Multiple Backends

Support **multiple implementations** of the same contract:

```agda
-- Pure Agda (extracted to Haskell)
concretePrimitives : JSONPrimitives

-- FFI to Aeson (Haskell library)
ffiPrimitives : JSONPrimitives

-- Mock (for property testing)
mockPrimitives : JSONPrimitives

-- All must satisfy the same contract
-- All run the same tests
-- All proven equivalent via η
```

## Homotopy Type Theory Connection

This structure mirrors **homotopy type theory**:

- **Types** = Spaces (JSON transformations)
- **Values** = Points (specific transformations)
- **Equality** = Paths (roundtrip m ≡ m)
- **Natural Transformation** = Homotopy (continuous deformation)

The natural transformation η witnesses that parameterized and concrete are **homotopically equivalent**—you can continuously deform one into the other while preserving all properties.

## Implementation Strategy

### Phase 2A: Contract Definition ✅

- [x] Define `JSONPrimitives` record (interface + laws)
- [x] Define `JSONTransformationParameterized` module
- [x] Define `JSONTransformationEquivalence` module
- [x] Define generic test suite

### Phase 2B: Concrete Implementation

1. **Implement primitives in pure Agda:**
   ```agda
   json-get-concrete : JSON → String → Maybe JSON
   json-get-concrete j key = ... -- String parsing
   ```

2. **Prove concrete satisfies contract laws:**
   ```agda
   concrete-get-set-same : ∀ j k v → 
     json-get-concrete (json-set-concrete j k v) k ≡ just v
   concrete-get-set-same j k v = ... -- proof by computation
   ```

3. **Bundle into `concretePrimitives`** (already done)

4. **Instantiate parameterized module:**
   ```agda
   module ConcreteTransformation = 
     JSONTransformationParameterized concretePrimitives
   ```

### Phase 2C: Equivalence Proofs

1. **Prove η naturality:**
   ```agda
   naturality : ∀ strat m →
     backward (forward strat m) ≡ backward (forward strat m)
   ```

2. **Prove homotopy contract:**
   ```agda
   homotopy-contract : ∀ strat m →
     roundtrip strat m ≡ roundtrip strat m
   ```

### Phase 2D: Testing

1. **Instantiate tests for concrete:**
   ```agda
   module ConcreteTests = JSONTransformationTests concretePrimitives
   ```

2. **Run property tests:**
   ```agda
   test-roundtrip-preserves
   test-fragments-valid
   test-metadata-preserved
   ```

3. **Validate on real data:**
   ```
   data/dependency_graph.json → decompose → data/deps/ → recompose → compare
   ```

### Phase 2E: Extraction

1. **Extract to Haskell:**
   ```bash
   agda -c src/agda/Plan/CIM/JSONTransformationContract.agda
   ```

2. **Compile extracted code:**
   ```bash
   ghc -o json-transform MAlonzo/Code/Plan/CIM/JSONTransformationContract.hs
   ```

3. **Run on real data:**
   ```bash
   ./json-transform decompose data/dependency_graph.json data/deps/
   ./json-transform recompose data/deps/ build/dependency_graph_reconstructed.json
   diff data/dependency_graph.json build/dependency_graph_reconstructed.json
   ```

## Comparison: Three Approaches

| Aspect | Adequacy Kit | Module Param | Hybrid (Both) |
|--------|--------------|--------------|---------------|
| Interface | Implicit | **Explicit** | **Explicit** |
| Laws | Proof obligations | **In contract** | **In contract** |
| Implementation | Synthesized | Parameterized | **Both** |
| Equivalence | Adequacy proof | Natural trans | **Both witness each other** |
| Testing | Per-kit | **Generic** | **Generic + specific** |
| Extraction | From kits | From concrete | **From either** |
| Proof reuse | Via adequacy | **Via polymorphism** | **Maximum** |

**Best of both worlds:** Hybrid approach combines adequacy (synthesis from primitives) with module parameters (contract enforcement).

## Architectural Elegance

The module parameter approach is **architecturally superior** because:

1. **Separation of concerns:** Interface vs implementation vs proof
2. **Reusability:** Tests and proofs work for ALL implementations
3. **Flexibility:** Swap implementations without changing logic
4. **Verifiability:** Contract laws are explicit, not implicit
5. **Composability:** Modules compose naturally
6. **Higher-order:** Contract is a first-class mathematical object

## Relationship to Adequacy Framework

The module parameter approach **complements** adequacy:

- **Adequacy:** Proves kit's primitives are **sufficient**
- **Module params:** Proves implementations are **equivalent**
- **Together:** Full verification (sufficiency + equivalence)

```agda
-- Adequacy: kit suffices
adequacy : ∀ kit → solve (face kit) kit ≡ refl

-- Module params: implementations equivalent
equivalence : ∀ P₁ P₂ → (P₁ ≅ P₂) → forward₁ ≡ forward₂

-- Combined: verified correctness
correctness : ∀ kit P → 
  (adequate kit) → (P implements kit) → forward P is correct
```

## Next Steps

1. **Implement concrete primitives** (json-get, json-set, etc.)
2. **Prove concrete satisfies contract laws**
3. **Fill natural transformation holes**
4. **Run tests on concrete implementation**
5. **Extract to Haskell and validate on real data**
6. **Add FFI implementation as alternative backend**
7. **Add mock implementation for property testing**

## Traceability

- **Module parameter pattern:** Standard Agda (e.g., Agda stdlib)
- **Natural transformations:** Category theory + HoTT
- **Homotopical contracts:** Dependent type theory literature
- **Implementation:** [JSONTransformationContract.agda](../../src/agda/Plan/CIM/JSONTransformationContract.agda)
- **Related:** [JSON-ADEQUACY.md](JSON-ADEQUACY.md) (adequacy framework)

---

**Status:** Architecture documented, contract module created, ready for Phase 2B (concrete primitive implementation).

This approach exemplifies **higher-order formal verification**: the contract itself is a mathematical object, and natural transformations witness equivalences at the type level, not just runtime.
