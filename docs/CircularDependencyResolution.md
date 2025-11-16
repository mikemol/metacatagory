# Circular Dependency Resolution in Algorithm Registry

## The Question

> "I thought that could be resolved by declaring with induction? If a nonterminal ends up cycling, we ought to be able to find where along that cycle an alternate interpretation is available, breaking the cycle."

## The Answer: It Depends on the Type of Circularity

You're absolutely right that **induction and coinduction** can resolve certain circular dependencies. However, the circularity we encountered was specifically in **instance search**, which operates differently.

---

## Three Types of Circularity

### 1. **Runtime Circularity (✓ Induction Works)**

**Example: Mutually Recursive Functions**
```agda
mutual
  even : Nat → Bool
  even zero = true
  even (suc n) = odd n
  
  odd : Nat → Bool
  odd zero = false
  odd (suc n) = even n
```

**Resolution:** Agda's termination checker proves these terminate via structural recursion.

**Example: Mutually Recursive Types**
```agda
mutual
  data Tree : Set where
    leaf : Tree
    node : Forest → Tree
  
  data Forest : Set where
    empty : Forest
    cons : Tree → Forest → Forest
```

**Resolution:** Agda accepts this because the cycle goes through positive positions.

**Example: Coinductive Types**
```agda
record Stream (A : Set) : Set where
  coinductive
  field
    head : A
    tail : Stream A  -- Self-reference!
```

**Resolution:** Coinduction allows productive infinite structures.

---

### 2. **Type-Level Circularity (✓ Induction Works with Care)**

**Example: Indexed Types with Mutual Dependencies**
```agda
mutual
  data FieldClass : FieldDeclaration → Set where
    FiniteField : {F : FieldDeclaration} → Evidence F FiniteFieldType → FieldClass F
    NumberField : {F : FieldDeclaration} → Evidence F NumberFieldType → FieldClass F
  
  Evidence : FieldDeclaration → FieldType → Set
  Evidence F FiniteFieldType = IsFiniteField F
  Evidence F NumberFieldType = IsNumberField F
```

**Resolution:** Mutual blocks allow co-defined types, as long as:
- Termination is structurally evident
- No circularity in positive positions

---

### 3. **Instance Search Circularity (✗ Induction Doesn't Apply)**

**The Problem We Hit:**
```agda
instance
  finiteFieldHasType : {F : FieldDeclaration} 
                     → ⦃ _ : IsFiniteField F ⦄  -- Needs an instance
                     → HasFieldType F           -- To produce an instance
```

**Why This Is Different:**

Instance resolution happens during **type-checking** (compile-time), not runtime:

1. **No Termination Checking**: Instance search isn't a runtime function, so Agda's termination checker doesn't apply
2. **Depth-Limited Search**: Agda uses bounded search to prevent infinite loops during type-checking
3. **Eager Evaluation**: Instance search happens during elaboration, not lazy evaluation

**The Circularity:**
```
Need: HasFieldType F
↓
Search for instances of HasFieldType F
↓
Find: finiteFieldHasType (requires instance of IsFiniteField F)
↓
Search for instances of IsFiniteField F
↓
(Hypothetically) Find: someInstance (requires instance of HasFieldType F)
↓
CYCLE! Instance search fails
```

---

## Our Solution: Explicit Constructors (Hybrid Dependent Pairs)

Instead of automatic instance search, we use **explicit helper functions**:

```agda
-- Dependent pair: field type tag + evidence
FieldClassification : FieldDeclaration → Set
FieldClassification F = Σ FieldType (FieldTypeEvidence F)

-- Explicit constructors (not instances!)
classifyAsFiniteField : (F : FieldDeclaration) → IsFiniteField F → FieldClassification F
classifyAsFiniteField F ev = (FiniteFieldType , ev)

classifyAsNumberField : (F : FieldDeclaration) → IsNumberField F → FieldClassification F
classifyAsNumberField F ev = (NumberFieldType , ev)

-- Dispatch using pattern matching on the dependent pair
dispatchBundle : (F E : FieldDeclaration) 
               → FieldClassification F 
               → FieldClassification E 
               → AlgorithmBundle F E
dispatchBundle F E (FiniteFieldType , evF) (FiniteFieldType , evE) = 
  finiteFieldBundle F E evF evE
dispatchBundle F E (NumberFieldType , evF) (NumberFieldType , evE) = 
  numberFieldBundle F E evF evE
dispatchBundle F E _ _ = 
  genericAlgorithmBundle F E
```

**Benefits:**
- ✓ No instance search cycle
- ✓ Explicit evidence flow (better for proof terms)
- ✓ Pattern matching works on dependent pairs
- ✓ Easy to extend (add new classifyAsXXX functions)
- ✓ Scales well (no instance resolution complexity)

**Usage:**
```agda
-- At call site, explicitly construct classifications
lookupGaloisGroupWithClassification Q GF8 
  (classifyAsNumberField Q nfEvidence) 
  (classifyAsFiniteField GF8 ffEvidence)
```

---

## When Could Induction Have Helped?

If our problem were:

**Scenario A: Recursive Evidence Construction**
```agda
-- Computing evidence for composite structures from parts
computeEvidence : (F : FieldDeclaration) → Evidence F
computeEvidence (primitive F) = primitiveEvidence F
computeEvidence (extension F E) = 
  combineEvidence (computeEvidence F) (computeEvidence E)
```

→ **Induction works!** Structural recursion on field declarations.

**Scenario B: Infinite Streams of Evidence**
```agda
record ProofStream (P : Set) : Set where
  coinductive
  field
    currentProof : P
    moreProofs : ProofStream P
```

→ **Coinduction works!** Productive infinite evidence.

**Scenario C: Mutually Defined Type Families**
```agda
mutual
  FieldEvidence : FieldType → Set
  FieldEvidence FiniteFieldType = FiniteFieldData
  FieldEvidence NumberFieldType = NumberFieldData
  
  data FiniteFieldData : Set where
    mkFF : (proof : FieldEvidence FiniteFieldType → Bool) → FiniteFieldData
```

→ **Mutual recursion works!** (if positivity checks pass)

---

## Summary

| Circularity Type    | Resolution Technique      | Applies Here?              |
| ------------------- | ------------------------- | -------------------------- |
| Recursive functions | Structural induction      | ✗ (not runtime)            |
| Recursive types     | Mutual blocks             | ✗ (not the issue)          |
| Infinite data       | Coinduction               | ✗ (not infinite)           |
| Instance search     | **Explicit constructors** | ✓ **This is what we used** |

**Your intuition about induction breaking cycles is correct** - but it applies to runtime/type-level recursion, not compile-time instance resolution.

The **explicit constructor pattern** we adopted gives us:
- The benefits of dependent pairs (explicit evidence)
- The benefits of type-driven dispatch (pattern matching on tags)
- **Without** the complexity of instance search

This is actually a **better design** for a library that others will extend, because:
1. Evidence flow is explicit and traceable
2. No "spooky action at a distance" from instance resolution
3. Easy to understand what's happening at call sites
4. Scales to arbitrary numbers of field types without instance search complexity

---

## Implementation Status

✓ **Completed**: Simplified hybrid approach using dependent pairs with explicit constructors
✓ **Typechecks**: Full project passes `make check`
✓ **Ready to use**: `lookupWithClassification` functions available

Next step: Create examples demonstrating the classification pattern in practice.
