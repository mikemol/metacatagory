{-# OPTIONS --without-K #-}

-- | Chapter 1 §7: 2-categorical scaffolding—hom-categories, 2-categories,
--   2-functors, and coherence data captured as declaration records.
module Chapter1.Level1sub7 where

open import Agda.Builtin.Unit     using (⊤; tt)
open import Agda.Builtin.String   using (String)
open import Agda.Builtin.List     using (List; []; _∷_)
open import Core.Phase using (Bool; true; false)
open import Agda.Builtin.Equality using (_≡_)

open import Metamodel as M
open import Core      as C

-- Lightweight expression placeholder
Expr7 : Set
Expr7 = String

------------------------------------------------------------------------
-- Part 1: 2-Categories (Section 7.1)
------------------------------------------------------------------------

-- CATEGORY: The hom-category C(A,B). Its existence, for every pair of objects (A,B),
-- is the defining feature of a 2-category.
record HomCategoryDeclaration : Set where
  constructor HOM_CATEGORY_has_OneMorphisms_TwoMorphisms
  field
    obj1 : M.Identifier
    obj2 : M.Identifier
    oneMorphisms : List M.Identifier  -- 1-morphisms as objects
    twoMorphisms : List M.Identifier  -- 2-morphisms as morphisms

-- CATEGORY: The horizontal composition functor.
record CompositionFunctorDeclaration : Set where
  constructor COMPOSITION_FUNCTOR_∘
  field
    source1 : HomCategoryDeclaration
    source2 : HomCategoryDeclaration
    target : HomCategoryDeclaration

-- CATEGORY: The identity-on-A functor.
record IdentityFunctorDeclaration : Set where
  constructor IDENTITY_FUNCTOR_Id
  field
    object : M.Identifier
    targetHomCat : HomCategoryDeclaration

-- CATEGORY: A category enriched over the category of small categories (Cat).
record TwoCategoryDeclaration : Set where
  constructor _2CATEGORY
  field
    name : M.Identifier
    objects : List M.Identifier
    homCategories : List HomCategoryDeclaration
    compositionFunctors : List CompositionFunctorDeclaration
    identityFunctors : List IdentityFunctorDeclaration

------------------------------------------------------------------------
-- Part 2: 2-Functors and 2-Natural Transformations (Section 7.2)
------------------------------------------------------------------------

-- CATEGORY: Mapping between hom-categories
record HomCategoryFunctorMapping : Set where
  constructor map_hom_cat_to_hom_cat_via_functor
  field
    sourceObj1 : M.Identifier
    sourceObj2 : M.Identifier
    targetObj1 : M.Identifier
    targetObj2 : M.Identifier
    functor : M.Identifier

-- CATEGORY: A strict 2-functor, or a homomorphism of 2-categories.
record TwoFunctorDeclaration : Set where
  constructor _2FUNCTOR
  field
    name : M.Identifier
    source : M.Identifier
    target : M.Identifier
    actionOnObjects : List M.Identifier
    actionOnHomCats : List HomCategoryFunctorMapping

-- CATEGORY: The assertion of the Pentagon Axiom for strict 2-categories.
record AssociativityAxiomTwoCat : Set where
  constructor AXIOM_Associativity
  field
    forAllQuads : M.Identifier  -- (A,B,C,D)
    equation : M.Identifier

-- CATEGORY: The assertion of the Triangle Axiom for strict 2-categories.
record UnitalityAxiomTwoCat : Set where
  constructor AXIOM_Unitality
  field
    forAllPairs : M.Identifier  -- (A,B)
    leftEquation : M.Identifier
    rightEquation : M.Identifier

------------------------------------------------------------------------
-- Part 3: Modifications and n-Categories (Section 7.3)
------------------------------------------------------------------------

-- CATEGORY: A 3-morphism in the 3-category of 2-categories.
record ModificationDeclaration : Set where
  constructor MODIFICATION
  field
    name : M.Identifier
    source : M.Identifier  -- 2-natural transformation α
    target : M.Identifier  -- 2-natural transformation β
    components : List M.Identifier  -- μ_X for each object X

-- CATEGORY: The assertion that the universe of (small) 2-categories forms a 3-category.
record CategoryLevelDeclaration : Set where
  constructor _is_a_3-CATEGORY
  field category : M.Identifier

-- CATEGORY: The inductive definition of an n-category.
record NCategoryRecursiveDefinition : Set where
  constructor DEFINE_n+1_CATEGORY_as_CategoryEnrichedOver_n-Cat
  field level : M.Identifier

------------------------------------------------------------------------
-- Part 4: 2-Limits and Bilimits (Section 7.4)
------------------------------------------------------------------------

-- CATEGORY: A cone in a 2-category whose commuting triangles hold with strict equality.
record StrictConeDeclaration : Set where
  constructor STRICT_CONE_over
  field
    diagram : M.Identifier
    apex : M.Identifier
    legs : List M.Identifier

-- CATEGORY: A cone where the triangles are witnessed to commute by specified,
-- invertible 2-cells.
record PseudoConeDeclaration : Set where
  constructor PSEUDO_CONE_over
  field
    diagram : M.Identifier
    apex : M.Identifier
    legs : List M.Identifier
    cells : List M.Identifier  -- isomorphisms witnessing commutativity

-- CATEGORY: The property of being terminal 'up to equivalence'.
record BiTerminalObjectProperty : Set where
  constructor _is_BI_TERMINAL_within
  field
    object : M.Identifier
    twoCategory : M.Identifier

-- CATEGORY: A limit in the underlying 1-category of the 2-category.
record StrictTwoLimitDeclaration : Set where
  constructor STRICT_TWO_LIMIT_of_is
  field
    diagram : M.Identifier
    limit : StrictConeDeclaration

-- CATEGORY: A limit over a strictly commuting diagram with weak universal property.
record PseudoLimitDeclaration : Set where
  constructor PSEUDO_LIMIT_of_is
  field
    diagram : M.Identifier
    limit : StrictConeDeclaration

-- CATEGORY: The most general and natural notion of a limit in a 2-category or bicategory.
record BilimitDeclaration : Set where
  constructor BILIMIT_of_is
  field
    diagram : M.Identifier
    limit : PseudoConeDeclaration

------------------------------------------------------------------------
-- Part 5: Lax Functors and Pseudo-Functors (Section 7.5)
------------------------------------------------------------------------

-- CATEGORY: Comparison cell for composition
record ComparisonCellComposition : Set where
  constructor φ_comp
  field
    morph1 : M.Identifier
    morph2 : M.Identifier

-- CATEGORY: Comparison cell for identity
record ComparisonCellIdentity : Set where
  constructor φ_id
  field object : M.Identifier

-- CATEGORY: The structural data for a lax functor.
record LaxFunctorData : Set where
  constructor DATA_LaxFunctor
  field
    name : M.Identifier
    source : M.Identifier
    target : M.Identifier
    actionOnObjects : List M.Identifier
    actionOn1Morphisms : List M.Identifier
    compositionCells : List ComparisonCellComposition
    identityCells : List ComparisonCellIdentity

-- CATEGORY: The coherence law for a lax functor's interaction with associativity.
record LaxAssociativityAxiom : Set where
  constructor AXIOM_LaxAssociativity_for
  field
    laxFunctorData : LaxFunctorData
    equation : M.Identifier

-- CATEGORY: The coherence law for a lax functor's interaction with identity.
record LaxUnitalityAxiom : Set where
  constructor AXIOM_LaxUnitality_for
  field
    laxFunctorData : LaxFunctorData
    equation : M.Identifier

-- CATEGORY: The fully-specified definition of a lax functor.
record LaxFunctorDeclaration : Set where
  constructor LAX_FUNCTOR_verifies
  field
    functorData : LaxFunctorData
    associativityProof : LaxAssociativityAxiom
    unitalityProof : LaxUnitalityAxiom

-- CATEGORY: A homomorphism of bicategories.
record PseudoFunctorDeclaration : Set where
  constructor PSEUDOFUNCTOR
  field
    name : M.Identifier
    source : M.Identifier
    target : M.Identifier
    underlyingLaxFunctor : LaxFunctorDeclaration

-- CATEGORY: A concrete realization of a lax functor via lax monoidal functors.
record LaxMonoidalFunctorAsLaxFunctor : Set where
  constructor INSTANCE_from_lax_monoidal_functor
  field laxMonoidalFunctor : M.Identifier

------------------------------------------------------------------------
-- Part 6: Lax Limits and Pseudo-Limits (Section 7.6)
------------------------------------------------------------------------

-- CATEGORY: The most general notion of a cone in a 2-categorical context.
record LaxConeDeclaration : Set where
  constructor LAX_CONE_over
  field
    diagram : M.Identifier
    apex : M.Identifier
    legs : List M.Identifier
    comparisonCells : List M.Identifier  -- not necessarily isomorphisms

-- CATEGORY: The universal lax cone.
record LaxLimitDeclaration : Set where
  constructor LAX_LIMIT_of_is
  field
    diagram : M.Identifier
    limit : LaxConeDeclaration

-- CATEGORY: The assertion of a clear hierarchy of limit concepts.
record LimitHierarchyTheorem : Set where
  constructor THEOREM_LimitHierarchy
  field unit : ⊤

------------------------------------------------------------------------
-- Part 7: Bicategories (Section 7.7)
------------------------------------------------------------------------

-- CATEGORY: The hom-category for a bicategory (functor category).
record BicategoryHomCategory : Set where
  constructor HOM_CATEGORY_Bicat
  field
    obj1 : M.Identifier
    obj2 : M.Identifier

-- CATEGORY: The horizontal composition functor for bicategories.
record BicategoryCompositionFunctor : Set where
  constructor ∘_ABC
  field
    objA : M.Identifier
    objB : M.Identifier
    objC : M.Identifier

-- CATEGORY: The identity functor for bicategories.
record BicategoryIdentityFunctor : Set where
  constructor Id_Bicat
  field object : M.Identifier

-- CATEGORY: The associator isomorphism.
record ConstraintAssociator : Set where
  constructor associator_alpha
  field naturalIso : M.Identifier

-- CATEGORY: The left unitor isomorphism.
record ConstraintLeftUnitor : Set where
  constructor left_unitor_lambda
  field naturalIso : M.Identifier

-- CATEGORY: The right unitor isomorphism.
record ConstraintRightUnitor : Set where
  constructor right_unitor_rho
  field naturalIso : M.Identifier

-- CATEGORY: The complete bicategory structure
record BicategoryDeclaration : Set where
  constructor BICATEGORY
  field
    name : M.Identifier
    objects : List M.Identifier
    homCategories : List BicategoryHomCategory
    compositionFunctors : List BicategoryCompositionFunctor
    identityFunctors : List BicategoryIdentityFunctor
    associator : ConstraintAssociator
    leftUnitor : ConstraintLeftUnitor
    rightUnitor : ConstraintRightUnitor

------------------------------------------------------------------------
-- Part 8: Distributors (Section 7.8)
------------------------------------------------------------------------

-- CATEGORY: A profunctor, or bimodule, from A to B.
record DistributorDeclaration : Set where
  constructor DISTRIBUTOR
  field
    name : M.Identifier
    source : M.Identifier
    target : M.Identifier

-- CATEGORY: The composition of distributors, defined by the coend.
record DistributorComposition : Set where
  constructor _∘_Dist
  field
    distributor1 : DistributorDeclaration
    distributor2 : DistributorDeclaration

-- CATEGORY: The bicategory of small categories, distributors, and natural transformations.
record BicategoryOfDistributors : Set where
  constructor Dist
  field unit : ⊤

------------------------------------------------------------------------
-- Part 9: Cauchy Completeness versus Distributors (Section 7.9)
------------------------------------------------------------------------

-- CATEGORY: The identity distributor on C, which is simply the hom-functor of C.
record IdentityDistributor : Set where
  constructor IdDist
  field category : M.Identifier

-- CATEGORY: The opposite or transpose of a distributor D.
record OppositeDistributor : Set where
  constructor _^op
  field distributor : DistributorDeclaration

-- CATEGORY: A deep characterization theorem connecting Cauchy completeness to
-- distributor factorization.
record IdempotentCompletenessAsDistributorFactorization : Set where
  constructor THEOREM_CauchyCompletenessViaDistributors
  field category : M.Identifier

------------------------------------------------------------------------
-- Bridge postulates: Connect axiom records to typed Proof witnesses
------------------------------------------------------------------------

open C using (Subject; AxiomName; Proof)
open C using (CategoryPropertyS; AdjunctionS)
open C using (TriangleIdentitiesName; SubobjectLatticeCompletenessName)

postulate
  -- 2-category associativity coherence bridge
  twoCatAssociativityProof
    : (ax : AssociativityAxiomTwoCat)
    -> (C : M.Identifier) -- identifier of the 2-category
    -> Proof (TwoCategoryS C) TwoCatAssociativityName

  -- 2-category unitality (triangle) coherence bridge
  twoCatUnitalityProof
    : (ax : UnitalityAxiomTwoCat)
    -> (C : M.Identifier)
    -> Proof (TwoCategoryS C) TwoCatUnitalityName

  -- Limit hierarchy meta-theorem bridge
  limitHierarchyProof
    : (thm : LimitHierarchyTheorem)
    -> (C : M.Identifier)
    -> Proof (CategoryPropertyS C) LimitHierarchyName

  -- Lax functor associativity coherence bridge
  laxAssociativityProof
    : (ax : LaxAssociativityAxiom)
    -> (F C D : M.Identifier) -- lax functor name, source, target
    -> Proof (LaxFunctorS F C D) LaxAssociativityName

  -- Lax functor unitality coherence bridge
  laxUnitalityProof
    : (ax : LaxUnitalityAxiom)
    -> (F C D : M.Identifier)
    -> Proof (LaxFunctorS F C D) LaxUnitalityName

  -- Bicategory overall coherence (associator/unitor constraints) bridge
  bicategoryCoherenceAssociativityProof
    : (bicat : BicategoryDeclaration)
    -> (B : M.Identifier)
    -> Proof (BicategoryS B) TwoCatAssociativityName

  bicategoryCoherenceUnitalityProof
    : (bicat : BicategoryDeclaration)
    -> (B : M.Identifier)
    -> Proof (BicategoryS B) TwoCatUnitalityName

  -- Cauchy completeness via distributors bridge
  cauchyViaDistributorsProof
    : (thm : IdempotentCompletenessAsDistributorFactorization)
    -> (C : M.Identifier)
    -> Proof (CategoryPropertyS C) CauchyViaDistributorsName

------------------------------------------------------------------------
-- Notes: Structural encoding of 2-Categories, Bicategories, and Distributors, 2-functors,
-- modifications, n-categories, 2-limits, bilimits, lax functors, pseudo-functors,
-- bicategories, distributors, and Cauchy completeness via distributors.
-- CATEGORY prose preserved as comments. Bridge postulates connect axiom
-- records to Core.Proof.
------------------------------------------------------------------------
