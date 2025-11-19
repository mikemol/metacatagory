-- Core.UniversalProperties: Categorical characterization of algebraic constructions
-- This module formalizes the universal properties underlying algorithmic interfaces

module Core.UniversalProperties where

open import Core
open import Metamodel as M
open import Algebra.Foundation
open import Algebra.Rings.Basic
open import Algebra.Fields.Basic

-- ============================================================================
-- Universal Mapping Properties (General Framework)
-- ============================================================================

-- A universal property is characterized by:
-- 1. An object with structure
-- 2. A universal morphism
-- 3. Uniqueness of factorization

record UniversalProperty {Obj : Set₁} (structure : Obj → Set₁) : Set₂ where
  field
    -- The universal object
    universal : Obj
    universalStructure : structure universal
    
    -- For any other object with compatible structure
    factorize : (X : Obj) → (s : structure X) → M.Identifier
    
    -- Uniqueness: any other morphism with the property factors uniquely
    unique : (X : Obj) → (s : structure X) → (f : M.Identifier) → M.Identifier

-- ============================================================================
-- Initial and Terminal Objects
-- ============================================================================

-- Initial object: unique morphism to every other object
record InitialObject : Set₁ where
  field
    initial : M.Identifier
    initialMorphism : (X : M.Identifier) → M.Identifier
    initialUnique : (X : M.Identifier) → (f : M.Identifier) → M.Identifier

-- Terminal object: unique morphism from every other object
record TerminalObject : Set₁ where
  field
    terminal : M.Identifier
    terminalMorphism : (X : M.Identifier) → M.Identifier
    terminalUnique : (X : M.Identifier) → (f : M.Identifier) → M.Identifier

-- ============================================================================
-- Products and Coproducts as Universal Properties
-- ============================================================================

-- Product: terminal object in the category of cones
record ProductProperty (A B : M.Identifier) : Set₁ where
  field
    product : M.Identifier
    π₁ : M.Identifier  -- A ← A × B
    π₂ : M.Identifier  -- B ← A × B
    
    -- Universal property: for any cone (X, f, g)
    mediating : (X : M.Identifier) → (f : M.Identifier) → (g : M.Identifier) → M.Identifier
    
    -- Commutativity
    π₁-commutes : (X : M.Identifier) → (f g : M.Identifier) → M.Identifier
    π₂-commutes : (X : M.Identifier) → (f g : M.Identifier) → M.Identifier
    
    -- Uniqueness
    mediating-unique : (X : M.Identifier) → (f g h : M.Identifier) → M.Identifier

-- Coproduct: initial object in the category of cocones
record CoproductProperty (A B : M.Identifier) : Set₁ where
  field
    coproduct : M.Identifier
    ι₁ : M.Identifier  -- A → A + B
    ι₂ : M.Identifier  -- B → A + B
    
    -- Universal property: for any cocone (X, f, g)
    comediating : (X : M.Identifier) → (f : M.Identifier) → (g : M.Identifier) → M.Identifier
    
    -- Commutativity
    ι₁-commutes : (X : M.Identifier) → (f g : M.Identifier) → M.Identifier
    ι₂-commutes : (X : M.Identifier) → (f g : M.Identifier) → M.Identifier
    
    -- Uniqueness
    comediating-unique : (X : M.Identifier) → (f g h : M.Identifier) → M.Identifier

-- ============================================================================
-- Equalizers and Coequalizers
-- ============================================================================

-- Equalizer: universal among morphisms making a parallel pair equal
record EqualizerProperty (A B : M.Identifier) (f g : M.Identifier) : Set₁ where
  field
    equalizer : M.Identifier
    equalize : M.Identifier  -- equalizer → A
    
    -- The equalizer property: f ∘ equalize = g ∘ equalize
    equalizes : M.Identifier
    
    -- Universal property
    mediating : (X : M.Identifier) → (h : M.Identifier) → M.Identifier
    mediating-commutes : (X : M.Identifier) → (h : M.Identifier) → M.Identifier
    mediating-unique : (X : M.Identifier) → (h k : M.Identifier) → M.Identifier

-- Coequalizer: dual to equalizer
record CoequalizerProperty (A B : M.Identifier) (f g : M.Identifier) : Set₁ where
  field
    coequalizer : M.Identifier
    coequalize : M.Identifier  -- B → coequalizer
    
    -- The coequalizer property
    coequalizes : M.Identifier
    
    -- Universal property
    comediating : (X : M.Identifier) → (h : M.Identifier) → M.Identifier
    comediating-commutes : (X : M.Identifier) → (h : M.Identifier) → M.Identifier
    comediating-unique : (X : M.Identifier) → (h k : M.Identifier) → M.Identifier

-- ============================================================================
-- Pullbacks and Pushouts
-- ============================================================================

-- Pullback: product in the slice category
record PullbackProperty (A B C : M.Identifier) (f : M.Identifier) (g : M.Identifier) : Set₁ where
  field
    pullback : M.Identifier
    π₁ : M.Identifier  -- pullback → A
    π₂ : M.Identifier  -- pullback → B
    
    -- Commutativity square
    commutes : M.Identifier  -- f ∘ π₁ = g ∘ π₂
    
    -- Universal property
    mediating : (X : M.Identifier) → (h : M.Identifier) → (k : M.Identifier) 
              → M.Identifier  -- proof that f ∘ h = g ∘ k
              → M.Identifier
    π₁-commutes : (X h k : M.Identifier) → (p : M.Identifier) → M.Identifier
    π₂-commutes : (X h k : M.Identifier) → (p : M.Identifier) → M.Identifier
    mediating-unique : (X h k : M.Identifier) → (p m : M.Identifier) → M.Identifier

-- Pushout: coproduct in the coslice category
record PushoutProperty (A B C : M.Identifier) (f : M.Identifier) (g : M.Identifier) : Set₁ where
  field
    pushout : M.Identifier
    ι₁ : M.Identifier  -- B → pushout
    ι₂ : M.Identifier  -- C → pushout
    
    -- Commutativity square
    commutes : M.Identifier  -- ι₁ ∘ f = ι₂ ∘ g
    
    -- Universal property
    comediating : (X : M.Identifier) → (h : M.Identifier) → (k : M.Identifier)
                → M.Identifier  -- proof that h ∘ f = k ∘ g
                → M.Identifier
    ι₁-commutes : (X h k : M.Identifier) → (p : M.Identifier) → M.Identifier
    ι₂-commutes : (X h k : M.Identifier) → (p : M.Identifier) → M.Identifier
    comediating-unique : (X h k : M.Identifier) → (p m : M.Identifier) → M.Identifier

-- ============================================================================
-- Field-Specific Universal Properties
-- ============================================================================

-- Minimal polynomial: terminal in category of monic polynomials vanishing at α
record MinimalPolynomialProperty (F E : FieldDeclaration) (α : M.Identifier) : Set₁ where
  field
    minPoly : M.Identifier
    
    -- The minimal polynomial vanishes at α
    vanishesAt : M.Identifier
    
    -- The minimal polynomial is monic
    isMonic : M.Identifier
    
    -- Universal property: any other monic polynomial vanishing at α
    -- is divisible by minPoly
    divides : (p : M.Identifier) → M.Identifier → M.Identifier → M.Identifier

-- Splitting field: initial field containing all roots
record SplittingFieldProperty (F : FieldDeclaration) (f : M.Identifier) : Set₁ where
  field
    splittingField : FieldDeclaration
    embedding : M.Identifier  -- F → splittingField
    
    -- All roots of f are in the splitting field
    hasAllRoots : M.Identifier
    
    -- Generated by roots: no proper subfield contains F and all roots
    generatedByRoots : M.Identifier
    
    -- Universal property: initial among fields containing F and all roots
    mediating : (E : FieldDeclaration) → (inc : M.Identifier) → (roots : M.Identifier) 
              → M.Identifier

-- Galois closure: minimal normal extension containing a given extension
record GaloisClosureProperty (F E : FieldDeclaration) : Set₁ where
  field
    galoisClosure : FieldDeclaration
    embeddingF : M.Identifier  -- F → galoisClosure
    embeddingE : M.Identifier  -- E → galoisClosure
    
    -- The closure is normal and separable over F
    isNormal : M.Identifier
    isSeparable : M.Identifier
    
    -- Universal property: initial among normal extensions containing E
    mediating : (K : FieldDeclaration) → (incF : M.Identifier) → (incE : M.Identifier)
              → (normal : M.Identifier) → M.Identifier

-- ============================================================================
-- Function Field Universal Properties
-- ============================================================================

-- Rational function field as a free construction
record RationalFunctionFieldProperty (F : FieldDeclaration) : Set₁ where
  field
    -- F(t): rational functions over F
    rationalFunctionField : FieldDeclaration
    
    -- Embedding F → F(t)
    embedding : M.Identifier
    
    -- The transcendental generator t
    generator : M.Identifier
    
    -- Universal property: F(t) is initial among field extensions with a transcendental element
    -- For any field K and F → K and transcendental α ∈ K, there exists unique F(t) → K
    mediating : (K : FieldDeclaration) → (φ : M.Identifier) → (α : M.Identifier) 
              → (transcendental : M.Identifier) → M.Identifier
    
    -- The mediating morphism sends t ↦ α
    sends-generator : (K : FieldDeclaration) → (φ α transcendental : M.Identifier) → M.Identifier
    
    -- Uniqueness: any other morphism sending t to α equals the mediating morphism
    unique : (K : FieldDeclaration) → (φ α transcendental : M.Identifier) → (ψ : M.Identifier) → M.Identifier

-- Function field extension degree as dimension
record FunctionFieldDegreeProperty (F K : FieldDeclaration) : Set₁ where
  field
    -- K/F is a function field extension
    baseFunctionField : M.Identifier
    
    -- Extension degree [K : F] as dimension of K-vector space over F
    degree : M.Identifier
    
    -- Basis as transcendence basis + algebraic generators
    transcendenceBasis : M.Identifier
    algebraicGenerators : M.Identifier
    
    -- Universal property: degree is preserved under field homomorphisms
    degreePreserving : (L : FieldDeclaration) → (φ : M.Identifier) → M.Identifier

-- ============================================================================
-- Adjunctions for Extension Operations
-- ============================================================================

-- Tensor product adjunction: - ⊗_F E ⊣ Hom_F(E, -)
-- This characterizes field extensions categorically
record TensorProductAdjunction (F E : FieldDeclaration) : Set₁ where
  field
    -- The functors
    tensorWith : FieldDeclaration → FieldDeclaration
    homFrom : FieldDeclaration → FieldDeclaration
    
    -- Natural bijection
    φ : (A B : FieldDeclaration) → M.Identifier → M.Identifier
    ψ : (A B : FieldDeclaration) → M.Identifier → M.Identifier
    
    -- Naturality
    φ-natural : (A B : FieldDeclaration) → (f g : M.Identifier) → M.Identifier
    ψ-natural : (A B : FieldDeclaration) → (f g : M.Identifier) → M.Identifier
    
    -- Isomorphism
    φ-ψ-inverse₁ : (A B : FieldDeclaration) → (f : M.Identifier) → M.Identifier
    φ-ψ-inverse₂ : (A B : FieldDeclaration) → (f : M.Identifier) → M.Identifier

-- Free construction adjunction: Free ⊣ Forgetful
-- This underlies polynomial rings, free modules, etc.
record FreeAdjunction (U : M.Identifier) (F : M.Identifier) : Set₁ where
  field
    -- Unit: X → U(F(X))
    unit : (X : M.Identifier) → M.Identifier
    
    -- Counit: F(U(A)) → A
    counit : (A : M.Identifier) → M.Identifier
    
    -- Universal property: lifts of functions
    lift : (X A : M.Identifier) → M.Identifier → M.Identifier
    
    -- Triangular identities
    triangle₁ : (X : M.Identifier) → M.Identifier
    triangle₂ : (A : M.Identifier) → M.Identifier

-- General adjunction F ⊣ G: natural isomorphism Hom(FX, Y) ≅ Hom(X, GY)
-- This is the most general form capturing the free-forgetful pattern
record Adjunction (C D : M.Identifier) (F G : M.Identifier) : Set₁ where
  field
    -- Functors
    leftAdjoint : M.Identifier   -- F : C → D
    rightAdjoint : M.Identifier  -- G : D → C
    
    -- Unit: Id_C ⇒ G∘F
    unit : (X : M.Identifier) → M.Identifier
    
    -- Counit: F∘G ⇒ Id_D
    counit : (Y : M.Identifier) → M.Identifier
    
    -- Natural bijection: Hom(FX, Y) → Hom(X, GY)
    φ : (X Y : M.Identifier) → M.Identifier → M.Identifier
    ψ : (X Y : M.Identifier) → M.Identifier → M.Identifier
    
    -- Triangle identities (adjunction coherence)
    -- (G∘ε) ∘ (η∘G) = id_G
    triangleLeft : (X : M.Identifier) → M.Identifier
    -- (ε∘F) ∘ (F∘η) = id_F
    triangleRight : (Y : M.Identifier) → M.Identifier
    
    -- Natural isomorphism witnesses
    φ-ψ-inverse₁ : (X Y : M.Identifier) → (f : M.Identifier) → M.Identifier
    φ-ψ-inverse₂ : (X Y : M.Identifier) → (g : M.Identifier) → M.Identifier

-- ============================================================================
-- Limits and Colimits (General)
-- ============================================================================

-- A limit is a terminal object in the category of cones
record LimitProperty (D : M.Identifier) : Set₁ where
  field
    limit : M.Identifier
    cone : M.Identifier  -- limit → D (cone morphisms)
    
    -- Universal property
    mediating : (X : M.Identifier) → (c : M.Identifier) → M.Identifier
    mediating-commutes : (X : M.Identifier) → (c : M.Identifier) → M.Identifier
    mediating-unique : (X : M.Identifier) → (c m : M.Identifier) → M.Identifier

-- A colimit is an initial object in the category of cocones
record ColimitProperty (D : M.Identifier) : Set₁ where
  field
    colimit : M.Identifier
    cocone : M.Identifier  -- D → colimit (cocone morphisms)
    
    -- Universal property
    comediating : (X : M.Identifier) → (c : M.Identifier) → M.Identifier
    comediating-commutes : (X : M.Identifier) → (c : M.Identifier) → M.Identifier
    comediating-unique : (X : M.Identifier) → (c m : M.Identifier) → M.Identifier

-- ============================================================================
-- Categorical Dispatch: Universal Property of Algorithm Selection
-- ============================================================================

-- The algorithm registry itself has a universal property!
-- Given a field type, select the "best" (most specific) algorithm

record AlgorithmSelectionProperty (F E : FieldDeclaration) : Set₁ where
  field
    -- Collection of available algorithms (ordered by specificity)
    algorithms : M.Identifier  -- List of algorithm bundles
    
    -- Selection function (returns most specific)
    select : M.Identifier → M.Identifier
    
    -- Universal property: selected algorithm is terminal in the
    -- preorder of "more specific than"
    isTerminal : (alg : M.Identifier) → M.Identifier
    
    -- Any more general algorithm factors through the selected one
    factorization : (general specific : M.Identifier) → M.Identifier
