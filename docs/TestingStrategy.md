# Testing Strategy

## Philosophy

Our testing approach focuses on **behavioral phase boundary validation** rather than traditional unit testing. We test the transitions between architectural stages where data changes representation, type, or abstraction level.

## What is a "Phase"?

A **phase** is a behavioral state in a data transformation pipeline:

- **Input state**: Data with certain properties/types (the "before")
- **Transformation**: A function or process that changes representation
- **Output state**: Data with different properties/types (the "after")  
- **Boundary**: The transition point being validated

### Phase Characteristics

1. **Type-level transitions**: Phases involve changes in type structure
2. **Abstraction shifts**: Moving between concrete/abstract representations
3. **Behavioral invariants**: Properties preserved across boundaries
4. **Testability**: Both sides and the transition are independently verifiable

## Test Suite Organization

### Structure

Each test suite is organized as:

```agda
module Tests.SuiteName where
  -- Infrastructure: postulated test data
  
  module Phase1-DescriptiveName where
    -- Tests for first behavioral boundary
    test-before-state : InputType
    test-transition : InputType → OutputType
    test-after-state : OutputType
    -- Invariant validation
  
  module Phase2-NextBoundary where
    -- Tests for second boundary (output of Phase1 becomes input)
    ...
```

### Current Test Suites

1. **DispatchBehaviorTests** (9 phases, ~400 lines)
   - Domain: Registry dispatch system
   - Coverage: Evidence → Classification → Dispatch → Invocation

2. **UniversalPropertyTests** (9 phases, ~377 lines)
   - Domain: Algorithm-UMP bridge
   - Coverage: Algorithms → Specifications → Categorical structures

3. **WitnessConstructionTests** (12 phases, ~400 lines)
   - Domain: Witness lifecycle
   - Coverage: Identifiers → Witnesses → Composite structures

4. **ErrorHandlingTests** (8 phases, lightweight)
   - Domain: Error-preventing phase boundaries
   - Coverage: Type-level validation, dispatch exhaustiveness, algorithm preconditions, witness invariants, automorphism typing, bundle structure, property specificity, subfield directionality

5. **PropertyRegistryTests** (3 phases, lightweight)
   - Domain: Stable property identifiers
   - Coverage: Identifier typing, generic consumption, stability under simple composition

6. **AlgorithmCompositionTests** (9 phases, ~300 lines)
   - Domain: Multi-step algorithm pipelines
   - Coverage: Algorithm output validity, two/three-step composition, bundle composition, invariant preservation, UMP composition, error propagation, profiled composition, dependent composition

7. **SerializationTests** (9 phases, ~280 lines)
   - Domain: Witness externalization
   - Coverage: Identifier/field/extension/polynomial/Galois group/bundle serialization, structure preservation, error handling, profiled serialization

8. **PerformanceBoundaryTests** (9 phases, ~300 lines)
   - Domain: Computational complexity tracking
   - Coverage: Complexity classification (constant/polynomial/exponential), boundary detection, resource estimation, optimization opportunities, profiled complexity

9. **Tests/Index.agda**
   - Unified entry point importing all test suites
   - Documents coverage summary

## Phase Boundary Examples

### Example 1: Evidence → Classification (DispatchBehaviorTests)

**Input Phase**: Raw evidence predicates
```agda
F2-ev : IsFiniteField F2
```

**Transformation**: Classification constructor
```agda
classifyAsFiniteField : IsFiniteField F → FieldClassification
```

**Output Phase**: Tagged classification
```agda
FiniteFieldType F2-ev : FieldClassification
```

**Tests validate**:
- Evidence type is correct
- Classification preserves field identity
- Classification is invertible (roundtrip property)

### Example 2: Algorithm → UMP (UniversalPropertyTests)

**Input Phase**: Computational algorithm
```agda
alg : MinimalPolynomialAlgorithm F E
```

**Transformation**: Universal property bridge
```agda
minimalPolynomialImplementsUniversality : 
  MinimalPolynomialAlgorithm F E → MinimalPolynomialProperty F E α
```

**Output Phase**: Categorical specification
```agda
prop : MinimalPolynomialProperty F E α
```

**Tests validate**:
- Algorithm implements specification
- Universal property holds (terminality)
- Morphism uniqueness

### Example 3: Identifier → Witness (WitnessConstructionTests)

**Input Phase**: String identifier
```agda
fieldId : M.Identifier
```

**Transformation**: Witness constructor
```agda
M.mkFieldDeclaration : M.Identifier → FieldDeclaration
```

**Output Phase**: Typed witness
```agda
F : FieldDeclaration
```

**Tests validate**:
- Identifier preserved in witness metadata
- Type safety enforced
- Witnesses compose correctly

## Testing Principles

### 1. No Mock Objects

We use **postulates** instead of concrete implementations:
- Tests validate type signatures and API contracts
- Avoids coupling to specific implementations
- Focuses on architectural boundaries, not implementation details

```agda
postulate
  F : FieldDeclaration
  alg : MinimalPolynomialAlgorithm F E
```

### 2. Phase Isolation

Each phase module tests one boundary:
- Clear separation of concerns
- Independent validation
- Easy to locate failures

### 3. Progression Through Pipeline

Phases are numbered sequentially following data flow:
- Phase N's output becomes Phase N+1's input
- End-to-end coverage of entire pipeline
- Natural test organization

### 4. Type Safety as Validation

Many tests simply demonstrate well-typed expressions:
```agda
test-type-safety : GaloisGroup F E
test-type-safety = ... -- If this typechecks, constraint is enforced
```

The type system enforces correctness; our tests document expected behavior.

### 5. Proof Placeholders

Where full proofs aren't implemented, we use postulates:
```agda
postulate
  test-terminal-property : (f : ...) → Unique f
```

This validates the **API contract** even when implementation is incomplete.

## Coverage Strategy

### What We Test

✅ **Type transitions** - Evidence → Classification → Bundle  
✅ **Abstraction boundaries** - Algorithm → UMP → Category  
✅ **Composition** - Witnesses combine correctly  
✅ **Dispatch correctness** - Routing and invocation  
✅ **Type safety** - Constraints enforced by type system  
✅ **Determinism** - Same inputs yield same outputs  

### What We Don't Test

❌ **Concrete computations** - Specific algorithm results  
❌ **Performance** - Efficiency or optimization  
❌ **Edge cases** - Boundary values or error conditions  
❌ **Implementation details** - Internal data structures  

We test **architectural correctness**, not computational correctness.

## Phase Boundary Catalog

### DispatchBehaviorTests (9 phases)

1. **Evidence Typing**: Type predicates are well-formed
2. **Classification Construction**: Evidence → FieldClassification
3. **Lazy Instance Construction**: Classifiable instances exist
4. **Dispatch Routing**: Classification → Bundle selection
5. **Auto Dispatch**: Instance arguments resolve correctly
6. **Bundle Extraction**: Bundle → Specific algorithms
7. **End-to-End Invocation**: Full pipeline execution
8. **Single Algorithm Lookup**: Helper function correctness
9. **Dispatch Determinism**: Stability and uniqueness

### UniversalPropertyTests (9 phases)

1. **Algorithm → UMP**: Implementations satisfy specifications
2. **UMP → Categorical**: Properties lift to category theory
3. **Free Constructions**: Adjunctions and universal arrows
4. **Galois Correspondence**: Natural isomorphisms
5. **Algorithm Selection**: Terminality in algorithm category
6. **Minimal Polynomial**: Terminal object property
7. **Splitting Field**: Initial object property
8. **Composition Preservation**: Functoriality
9. **Bridge Coherence**: Commutative diagrams

### WitnessConstructionTests (12 phases)
### ErrorHandlingTests (8 phases)

1. Type-Level Validation: Evidence typing and classifier arity
2. Dispatch Routing: Exhaustive case handling in bundle selection
3. Algorithm Preconditions: Minimal polynomial and Galois group contexts
4. Witness Construction: Field/extension constructors enforce invariants
5. Automorphism Typing: Base/extension domains preserved
6. Bundle Extraction: Structure respected during algorithm retrieval
7. Property Specificity: Minimal polynomial property bound to (F, E, α)
8. Subfield Directionality: Inclusion E ⊇ F enforced by types

### PropertyRegistryTests (3 phases)

1. Identifier Typing: All registry entries are M.Identifier
2. Generic Consumption: Usable anywhere an Identifier is required
3. Structural Composition: Pairings of identifiers remain well-typed

### AlgorithmCompositionTests (9 phases)

1. Single Algorithm Validity: Individual outputs are well-typed
2. Two-Step Composition: Output → input compatibility
3. Three-Step Pipeline: End-to-end correctness
4. Bundle Composition: Algorithms from same bundle compose
5. Invariant Preservation: Properties maintained through composition
6. Universal Property Composition: Composite UMPs hold
7. Error Propagation: Maybe composition and fallback
8. Profiled Composition: Metadata tracking through pipelines
9. Dependent Composition: Type-indexed algorithm chains

### SerializationTests (9 phases)

1. Identifier Serialization: String conversion and parsing
2. Field Serialization: External representation of fields
3. Extension Serialization: Structure preservation
4. Polynomial Serialization: String format conversion
5. Galois Group Serialization: Group structure externalization
6. Bundle Serialization: Full algorithm suite persistence
7. Structure Preservation: Mathematical properties maintained
8. Deserialization Errors: Invalid data rejection
9. Profiled Serialization: Performance tracking

### PerformanceBoundaryTests (9 phases)

1. Constant Complexity: Identifier operations
2. Polynomial Complexity: Field classification
3. Exponential Complexity: Minimal polynomial computation
4. Complexity Boundaries: Where jumps occur in pipelines
5. Logarithmic Complexity: Bundle dispatch
6. Resource Estimation: Time and space costs
7. Optimization Opportunities: Fast/slow path identification
8. Factorial Complexity: Galois group enumeration
9. Profiled Complexity: Static and dynamic tracking

### WitnessConstructionTests (12 phases)

1. **Identifier Creation**: String → M.Identifier
2. **Field Construction**: Identifier → FieldDeclaration
3. **Extension Building**: (F, E) → Extension witnesses
4. **Polynomial Witnesses**: Elements and minimal polynomials
5. **Splitting Fields**: Polynomial → SplittingField
6. **Galois Groups**: Extension → Automorphism group
7. **Automorphism Witnesses**: Individual group elements
8. **Composite Structures**: Multiple witnesses combine
9. **Normal Closures**: Composite Galois theory
10. **Type Safety**: Constraints enforced (F ⊆ E)
11. **Homomorphism Witnesses**: Field → Field maps
12. **Subfield Verification**: Inclusion relationships

## Running Tests

### Individual Suite
```bash
agda --no-main -i src/agda src/agda/Tests/DispatchBehaviorTests.agda
agda --no-main -i src/agda src/agda/Tests/UniversalPropertyTests.agda
agda --no-main -i src/agda src/agda/Tests/WitnessConstructionTests.agda
agda --no-main -i src/agda src/agda/Tests/ErrorHandlingTests.agda
agda --no-main -i src/agda src/agda/Tests/PropertyRegistryTests.agda
agda --no-main -i src/agda src/agda/Tests/AlgorithmCompositionTests.agda
agda --no-main -i src/agda src/agda/Tests/SerializationTests.agda
agda --no-main -i src/agda src/agda/Tests/PerformanceBoundaryTests.agda
```

### All Tests
```bash
agda --no-main -i src/agda src/agda/Tests/Index.agda
```

Success = typechecking completes without errors.

## Future Directions

### Potential Formalizations

1. **Phase as a Type**
   ```agda
   record Phase (A B : Set) : Set where
     field
       before : A
       transform : A → B
       after : B
       preserves : Property A → Property B
   ```

2. **Phase Composition**
   ```agda
   _⟫_ : Phase A B → Phase B C → Phase A C
   ```

3. **Category of Phases**
   - Objects: System states
   - Morphisms: Phase transitions
   - Composition: Pipeline construction

4. **Behavioral Invariants**
   - Formal specification of properties preserved across boundaries
   - Automated invariant checking

### Additional Test Coverage

- **Error handling phases**: How errors propagate through pipeline
- **Optimization phases**: Transformation preservation under optimization
- **Serialization phases**: Witness → External representation
- **Interactive phases**: User input → System state changes

## Maintenance

### Adding New Tests

1. Identify a behavioral boundary in the system
2. Determine input/output types
3. Create `PhaseN-DescriptiveName` module
4. Write tests for before-state, transition, after-state
5. Validate invariants preserved
6. Update this document's catalog

### When Tests Fail

1. Identify which phase boundary failed
2. Check if API contract changed (expected)
3. Or if implementation broke invariant (bug)
4. Update tests if contract evolved
5. Fix implementation if invariant violated

### Test Organization Guidelines

- Keep phases focused on single boundaries
- Name phases descriptively (not just "Phase1")
- Document what invariant each phase validates
- Group related phases in same suite
- Number phases by data flow order

---

**Last Updated**: November 16, 2025  
**Test Suite Statistics**: 68 phases across 8 suites, ~2100 lines total
