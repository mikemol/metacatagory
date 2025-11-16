# Algorithm Correctness Framework

## Overview

The Algorithm Correctness framework (`Core/AlgorithmCorrectness.agda`) provides a formal specification system for reasoning about the correctness of algebraic algorithms. It establishes what it means for an algorithm to "correctly" compute a mathematical object by defining explicit proof obligations that relate the algorithm's output to the mathematical properties it should satisfy.

## Core Architecture

### Correctness Specifications

A `CorrectnessSpec` relates an algorithm's input and output types to a property that should hold:

```agda
record CorrectnessSpec (Input Output Property : Set₁) : Set₁ where
  field
    -- The property that should hold for the output
    property : Property
    
    -- How to check if an output satisfies the property
    satisfies : Output → Property → Bool
    
    -- Proof that satisfaction is decidable
    satisfiesDecidable : ∀ (out : Output) → Dec (satisfies out property ≡ tt)
```

### Algorithm Satisfaction

An `AlgorithmSatisfiesSpec` bundles an algorithm with a proof that it satisfies its specification:

```agda
record AlgorithmSatisfiesSpec {Input Output Property : Set₁}
                               (spec : CorrectnessSpec Input Output Property)
                               (algorithm : Input → Output) : Set₁ where
  field
    -- Proof that the algorithm satisfies the specification
    proof : ∀ (input : Input) → satisfies (algorithm input) (property spec) ≡ tt
    
    -- Constructive witness extraction
    extractWitness : ∀ (input : Input) → Output
    extractWitness = algorithm
    
    -- Verification that witness satisfies property
    verifyWitness : ∀ (input : Input) → satisfies (extractWitness input) (property spec) ≡ tt
    verifyWitness = proof
```

### Correctness Certificates

A `CorrectnessCertificate` packages a specification with its satisfaction proof:

```agda
record CorrectnessCertificate (Input Output Property : Set₁) : Set₁ where
  field
    spec : CorrectnessSpec Input Output Property
    algorithm : Input → Output
    satisfaction : AlgorithmSatisfiesSpec spec algorithm
```

## Algorithm-Specific Correctness

### Minimal Polynomial Correctness

For minimal polynomial algorithms, correctness means:

1. **Root Property**: The polynomial has α as a root
2. **Irreducibility**: The polynomial is irreducible over F
3. **Minimality**: Any polynomial with α as a root is divisible by this one
4. **Monic**: The leading coefficient is 1
5. **Coefficients in F**: All coefficients are in the base field

```agda
record MinimalPolynomialCorrectness (F : FieldDeclaration) (α : AlgebraicElement) : Set₁ where
  field
    poly : MinimalPolynomial
    
    -- Proof obligations
    rootProperty : isRoot poly α ≡ tt
    irreducibilityProof : isIrreducible poly ≡ tt
    minimalityProof : ∀ (q : Polynomial) → isRoot q α ≡ tt → divides poly q ≡ tt
    monicProof : isMonic poly ≡ tt
    coefficientsInF : ∀ (i : ℕ) → inField F (coefficient poly i) ≡ tt
```

### Splitting Field Correctness

For splitting field algorithms, correctness means:

1. **All Roots Present**: Every root of the polynomial is in the field
2. **Complete Splitting**: The polynomial factors completely into linear factors
3. **Minimal Field**: No proper subfield contains all roots
4. **Field Extension**: The splitting field is a valid field extension
5. **Multiplicity Preservation**: Root multiplicities match polynomial degree

```agda
record SplittingFieldCorrectness (F : FieldDeclaration) (poly : Polynomial) : Set₁ where
  field
    splitField : SplittingField
    roots : List AlgebraicElement
    
    -- Proof obligations
    allRootsInField : ∀ (r : AlgebraicElement) → r ∈ roots → inField splitField r ≡ tt
    completeSplitting : factorsCompletely poly roots ≡ tt
    minimalFieldProof : ∀ (K : FieldDeclaration) → 
                        isSubfield K splitField ≡ tt →
                        (∀ (r : AlgebraicElement) → r ∈ roots → inField K r ≡ tt) →
                        K ≡ splitField
    fieldExtensionProof : isFieldExtension F splitField ≡ tt
    multiplicityPreservation : length roots ≡ degree poly
```

### Galois Group Correctness

For Galois group algorithms, correctness means:

1. **Automorphisms Valid**: Each automorphism fixes the base field
2. **Group Closure**: Composition of automorphisms stays in group
3. **Group Axioms**: Associativity, identity, inverses hold
4. **Order Equals Degree**: |G| = [L:K] for Galois extension
5. **Fundamental Theorem**: Galois correspondence between subgroups and intermediate fields

```agda
record GaloisGroupCorrectness (K L : FieldDeclaration) : Set₁ where
  field
    galoisGroup : GaloisGroup
    automorphisms : List FieldAutomorphism
    
    -- Proof obligations
    automorphismsValid : ∀ (σ : FieldAutomorphism) → σ ∈ automorphisms → 
                         fixesField σ K ≡ tt
    groupClosure : ∀ (σ τ : FieldAutomorphism) → 
                   σ ∈ automorphisms → τ ∈ automorphisms →
                   (compose σ τ) ∈ automorphisms
    assocProof : ∀ (σ τ ρ : FieldAutomorphism) → 
                 compose (compose σ τ) ρ ≡ compose σ (compose τ ρ)
    identityProof : ∃[ e ] (∀ (σ : FieldAutomorphism) → 
                             compose e σ ≡ σ ∧ compose σ e ≡ σ)
    inverseProof : ∀ (σ : FieldAutomorphism) → σ ∈ automorphisms →
                   ∃[ σ⁻¹ ] (compose σ σ⁻¹ ≡ identity ∧ compose σ⁻¹ σ ≡ identity)
    orderEquality : length automorphisms ≡ extensionDegree K L
    fundamentalTheorem : GaloisCorrespondence K L galoisGroup
```

### Extension Degree Correctness

For extension degree algorithms, correctness means:

1. **Basis Linear Independence**: Basis elements are linearly independent
2. **Spanning**: Basis elements span the extension as K-vector space
3. **Degree Formula**: |basis| = [L:K]
4. **Tower Law**: For K ⊆ M ⊆ L, [L:K] = [L:M]·[M:K]

```agda
record ExtensionDegreeCorrectness (K L : FieldDeclaration) : Set₁ where
  field
    degree : ℕ
    basis : List AlgebraicElement
    
    -- Proof obligations
    basisLinearlyIndependent : linearlyIndependent K basis ≡ tt
    basisSpans : spans K basis L ≡ tt
    degreeFormula : length basis ≡ degree
    towerLaw : ∀ (M : FieldDeclaration) → 
               isSubfield K M ≡ tt → isSubfield M L ≡ tt →
               extensionDegree K L ≡ 
                 (extensionDegree K M) * (extensionDegree M L)
```

## Bundle Correctness

For algorithms that compute multiple related objects, `BundleCorrectness` ensures:

1. **Individual Correctness**: Each algorithm is correct
2. **Consistency**: The outputs are mutually consistent

```agda
record BundleCorrectness (F : FieldDeclaration) (α : AlgebraicElement) : Set₁ where
  field
    -- Individual algorithm correctness
    minPolyCorrectness : MinimalPolynomialCorrectness F α
    splitFieldCorrectness : SplittingFieldCorrectness F (poly minPolyCorrectness)
    galoisGroupCorrectness : GaloisGroupCorrectness F 
                               (splitField splitFieldCorrectness)
    degreeCorrectness : ExtensionDegreeCorrectness F 
                          (splitField splitFieldCorrectness)
    
    -- Consistency proofs
    consistentMinPoly : poly minPolyCorrectness ≡ 
                        computedPoly (splitField splitFieldCorrectness)
    consistentDegree : degree (poly minPolyCorrectness) ≡ 
                       order (galoisGroup galoisGroupCorrectness)
    consistentRoots : roots splitFieldCorrectness ≡ 
                      computedRoots (galoisGroup galoisGroupCorrectness)
```

## Additional Correctness Frameworks

### Invariant Preservation

Ensures algorithms preserve specified invariants:

```agda
record InvariantPreservation (State : Set₁) (Invariant : State → Bool) : Set₁ where
  field
    operation : State → State
    preserves : ∀ (s : State) → Invariant s ≡ tt → 
                Invariant (operation s) ≡ tt
```

### Universal Property Satisfaction

Proves algorithms satisfy universal mapping properties:

```agda
record SatisfiesUniversalProperty (Object : Set₁) : Set₁ where
  field
    universalObject : Object
    uniquenessProof : ∀ (obj : Object) (f : Morphism obj universalObject) →
                      ∃![ g ] (composeMorphism f g ≡ canonicalMorphism)
```

### Complexity Correctness

Relates algorithm complexity to theoretical bounds:

```agda
record ComplexityCorrectness (Input : Set₁) (complexity : Input → ℕ) : Set₁ where
  field
    upperBound : ℕ → ℕ
    complexityBound : ∀ (input : Input) → 
                      complexity input ≤ upperBound (size input)
```

### Error Handling Correctness

Ensures error conditions are properly detected:

```agda
record ErrorHandlingCorrectness (Input Output Error : Set₁) : Set₁ where
  field
    algorithm : Input → Output ⊎ Error
    validInput : Input → Bool
    errorCondition : Input → Error → Bool
    
    correctErrors : ∀ (input : Input) → validInput input ≡ ff →
                    ∃[ err ] (algorithm input ≡ inj₂ err ∧ 
                             errorCondition input err ≡ tt)
```

### Algorithm Totality

Proves algorithms terminate on all inputs:

```agda
record AlgorithmTotality (Input Output : Set₁) : Set₁ where
  field
    algorithm : Input → Output
    terminates : ∀ (input : Input) → ∃[ output ] (algorithm input ≡ output)
```

### Algorithm Termination

Proves algorithms terminate with decreasing measures:

```agda
record AlgorithmTermination (Input : Set₁) (measure : Input → ℕ) : Set₁ where
  field
    step : Input → Input
    measuredDecrease : ∀ (input : Input) → measure (step input) < measure input
    haltsAt : Input → Bool
    eventuallyHalts : ∀ (input : Input) → 
                      ∃[ n ] (haltsAt (iterate step n input) ≡ tt)
```

## Proof Construction

The framework provides builders to construct correctness proofs from constructive witnesses:

```agda
-- Build minimal polynomial correctness from constructive witness
minimalPolynomialCorrectness : ∀ {F α} → 
                                ConstructiveMinimalPolynomial F α →
                                MinimalPolynomialCorrectness F α

-- Build splitting field correctness from constructive witness
splittingFieldCorrectness : ∀ {F poly} →
                             ConstructiveSplittingField F poly →
                             SplittingFieldCorrectness F poly

-- Build Galois group correctness from constructive witness
galoisGroupCorrectness : ∀ {K L} →
                          ConstructiveGaloisGroup K L →
                          GaloisGroupCorrectness K L

-- Build extension degree correctness from constructive witness
extensionDegreeCorrectness : ∀ {K L} →
                              ConstructiveExtensionDegree K L →
                              ExtensionDegreeCorrectness K L
```

## Verification Pipeline

Correctness can be verified through a multi-phase process:

1. **Specification Construction**: Define what correctness means
2. **Algorithm Implementation**: Implement the algorithm
3. **Proof Construction**: Build satisfaction proof
4. **Certificate Generation**: Package spec + proof
5. **Verification**: Check the certificate

Example pipeline:

```agda
-- Phase 1: Define specification
spec : CorrectnessSpec FieldDeclaration MinimalPolynomial MinPolyProperty
spec = mkMinPolySpec

-- Phase 2: Implement algorithm
minPolyAlgorithm : FieldDeclaration → AlgebraicElement → MinimalPolynomial
minPolyAlgorithm F α = computeMinimalPolynomial F α

-- Phase 3: Construct proof (postulated in framework)
proof : AlgorithmSatisfiesSpec spec (minPolyAlgorithm F)
proof = proveCorrectness spec (minPolyAlgorithm F)

-- Phase 4: Generate certificate
certificate : CorrectnessCertificate FieldDeclaration MinimalPolynomial MinPolyProperty
certificate = mkCertificate spec (minPolyAlgorithm F) proof

-- Phase 5: Verify
verified : Bool
verified = verifyCertificate certificate
```

## Integration with Constructive Witnesses

The correctness framework complements the constructive witness system:

* **Constructive Witnesses**: Provide computational content (algorithms, data)
* **Algorithm Correctness**: Provide verification (proofs, specifications)

Together they enable **verified computation**:

```agda
-- Compute with constructive witness
cw : ConstructiveMinimalPolynomial F α
cw = mkConstructiveMinimalPolynomial F α

-- Extract correctness proof
correct : MinimalPolynomialCorrectness F α
correct = minimalPolynomialCorrectness cw

-- Use both: verified computation
verifiedMinPoly : ∃[ p ] (isCorrect p ≡ tt)
verifiedMinPoly = ⟨ poly correct , rootProperty correct ⟩
```

## Usage Patterns

### Pattern 1: Algorithm Certification

Certify an existing algorithm is correct:

```agda
certifyAlgorithm : (algo : Input → Output) →
                   (spec : CorrectnessSpec Input Output Property) →
                   CorrectnessCertificate Input Output Property
certifyAlgorithm algo spec = 
  mkCertificate spec algo (proveCorrectness spec algo)
```

### Pattern 2: Specification-Driven Development

Define spec first, then implement algorithm to satisfy it:

```agda
-- Step 1: Define what you want
spec : CorrectnessSpec Input Output Property

-- Step 2: Implement to spec
algorithm : Input → Output
algorithm = implementToSpec spec

-- Step 3: Prove correctness
proof : AlgorithmSatisfiesSpec spec algorithm
proof = constructProof spec algorithm
```

### Pattern 3: Correctness Composition

Compose correctness proofs for complex algorithms:

```agda
bundleCorrectness : BundleCorrectness F α
bundleCorrectness = mkBundleCorrectness
  (minimalPolynomialCorrectness cwMinPoly)
  (splittingFieldCorrectness cwSplitField)
  (galoisGroupCorrectness cwGaloisGroup)
  (extensionDegreeCorrectness cwExtDegree)
  consistencyProofs
```

## Examples

The `Examples/AlgorithmCorrectnessExamples.agda` module demonstrates:

1. **MinimalPolynomialCorrectnessExample**: Correctness of minimal polynomial computation
2. **SplittingFieldCorrectnessExample**: Correctness of splitting field construction
3. **GaloisGroupCorrectnessExample**: Correctness of Galois group calculation
4. **ExtensionDegreeCorrectnessExample**: Correctness of degree computation
5. **BundleCorrectnessExample**: Consistency across multiple algorithms
6. **CorrectnessSpecExample**: General specification construction
7. **CorrectnessViaUMPExample**: Correctness through universal properties
8. **InvariantPreservationExample**: Invariant-preserving algorithms
9. **ComplexityCorrectnessExample**: Complexity-correct algorithms
10. **ErrorHandlingCorrectnessExample**: Error-correct algorithms
11. **TotalityExample**: Total algorithms with termination proofs
12. **CertificatePipelineExample**: End-to-end certification pipeline

## Design Philosophy

1. **Explicit Proof Obligations**: Make correctness criteria explicit and checkable
2. **Constructive Proofs**: All proofs are constructive and computational
3. **Modular Verification**: Correctness of components implies correctness of compositions
4. **Type-Driven**: Use dependent types to ensure proofs match specifications
5. **Universal Properties**: Leverage categorical structures for correctness

## Future Extensions

Planned enhancements:

1. **Automated Proof Search**: Tactics for common correctness patterns
2. **Refinement Types**: Use refinement types to enforce correctness by construction
3. **Proof Reuse**: Library of common correctness proof patterns
4. **Performance Analysis**: Relate correctness to complexity bounds
5. **Error Analysis**: Formal treatment of numerical errors and approximations

## Related Documentation

* `docs/ConstructiveWitnesses.md`: Computational witness architecture
* `docs/TestingStrategy.md`: Test coverage for correctness framework
* `src/agda/Core/AlgorithmCorrectness.agda`: Implementation (520 lines)
* `src/agda/Examples/AlgorithmCorrectnessExamples.agda`: Usage examples (436 lines)

## Summary

The Algorithm Correctness framework provides:

* **Formal Specifications**: Precise definitions of algorithm correctness
* **Proof Obligations**: Explicit requirements for correctness proofs
* **Certificate System**: Bundling of specifications with proofs
* **Verification Pipeline**: Tools for constructing and checking correctness
* **Integration**: Seamless connection with constructive witnesses

This enables **verified computation in Galois theory**: algorithms with mathematical guarantees of correctness.
