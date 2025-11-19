-- Core.AlgorithmComplexity: Complexity classification for algorithmic operations
--
-- Phase III.1 (3.3): Complexity Classification and Measurement
-- Provides indexed properties linking algorithms to computational complexity classes,
-- establishing the reference point for efficiency measurement.

module Core.AlgorithmComplexity where

open import Agda.Builtin.String using (String)
open import Agda.Builtin.Unit using (⊤)
open import Agda.Primitive using (Level; _⊔_)
import Metamodel as M

-- Empty type for ordering relation
data ⊥ : Set where

-- ============================================================================
-- Complexity Classes
-- ============================================================================

-- Standard computational complexity hierarchy
data ComplexityClass : Set where
  Constant     : ComplexityClass  -- O(1)
  Logarithmic  : ComplexityClass  -- O(log n)
  Linear       : ComplexityClass  -- O(n)
  Linearithmic : ComplexityClass  -- O(n log n)
  Quadratic    : ComplexityClass  -- O(n²)
  Cubic        : ComplexityClass  -- O(n³)
  Polynomial   : ComplexityClass  -- O(nᵏ) for k > 3
  Exponential  : ComplexityClass  -- O(2ⁿ)
  Factorial    : ComplexityClass  -- O(n!)
  Unknown      : ComplexityClass  -- Complexity not yet analyzed

-- ============================================================================
-- Complexity Annotation
-- ============================================================================

-- Indexed complexity annotation linking an identifier to its complexity class
record ComplexityAnnotation : Set where
  field
    algorithmId : M.Identifier
    complexity  : ComplexityClass
    description : String  -- Human-readable complexity explanation
    assumptions : String  -- Assumptions (e.g., "for degree n polynomial")

-- Helper: Create complexity annotation
mkComplexityAnnotation : M.Identifier → ComplexityClass → String → String → ComplexityAnnotation
mkComplexityAnnotation id cc desc assump = record
  { algorithmId = id
  ; complexity  = cc
  ; description = desc
  ; assumptions = assump
  }

-- ============================================================================
-- Complexity-Annotated Algorithm Interface
-- ============================================================================

-- Generic wrapper adding complexity annotation to any algorithm interface
record AnnotatedAlgorithm {ℓ : Level} (AlgorithmType : Set ℓ) : Set ℓ where
  field
    algorithm   : AlgorithmType
    annotation  : ComplexityAnnotation

-- Helper: Annotate an algorithm instance
annotateAlgorithm : {ℓ : Level}{A : Set ℓ} → A → ComplexityAnnotation → AnnotatedAlgorithm A
annotateAlgorithm alg ann = record { algorithm = alg ; annotation = ann }

-- ============================================================================
-- Complexity Comparison and Validation
-- ============================================================================

-- Ordering on complexity classes (for comparison)
_≤ᶜ_ : ComplexityClass → ComplexityClass → Set
Constant     ≤ᶜ _            = ⊤
Logarithmic  ≤ᶜ Constant     = ⊥
Logarithmic  ≤ᶜ _            = ⊤
Linear       ≤ᶜ Constant     = ⊥
Linear       ≤ᶜ Logarithmic  = ⊥
Linear       ≤ᶜ _            = ⊤
Linearithmic ≤ᶜ Constant     = ⊥
Linearithmic ≤ᶜ Logarithmic  = ⊥
Linearithmic ≤ᶜ Linear       = ⊥
Linearithmic ≤ᶜ _            = ⊤
Quadratic    ≤ᶜ Polynomial   = ⊤
Quadratic    ≤ᶜ Exponential  = ⊤
Quadratic    ≤ᶜ Factorial    = ⊤
Quadratic    ≤ᶜ Unknown      = ⊤
Quadratic    ≤ᶜ _            = ⊥
Cubic        ≤ᶜ Polynomial   = ⊤
Cubic        ≤ᶜ Exponential  = ⊤
Cubic        ≤ᶜ Factorial    = ⊤
Cubic        ≤ᶜ Unknown      = ⊤
Cubic        ≤ᶜ _            = ⊥
Polynomial   ≤ᶜ Exponential  = ⊤
Polynomial   ≤ᶜ Factorial    = ⊤
Polynomial   ≤ᶜ Unknown      = ⊤
Polynomial   ≤ᶜ _            = ⊥
Exponential  ≤ᶜ Factorial    = ⊤
Exponential  ≤ᶜ Unknown      = ⊤
Exponential  ≤ᶜ _            = ⊥
Factorial    ≤ᶜ Unknown      = ⊤
Factorial    ≤ᶜ _            = ⊥
Unknown      ≤ᶜ Unknown      = ⊤
Unknown      ≤ᶜ _            = ⊥

-- Extract complexity class from annotation
getComplexity : ComplexityAnnotation → ComplexityClass
getComplexity = ComplexityAnnotation.complexity

-- Extract algorithm identifier from annotation
getAlgorithmId : ComplexityAnnotation → M.Identifier
getAlgorithmId = ComplexityAnnotation.algorithmId
