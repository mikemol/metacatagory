module Chapter2.Level2sub3 where

open import Agda.Builtin.Unit     using (⊤; tt)
open import Agda.Builtin.String   using (String)
open import Agda.Builtin.List     using (List; []; _∷_)
open import Agda.Builtin.Bool     using (Bool)
open import Agda.Builtin.Equality using (_≡_)

open import Metamodel as M
open import Core      as C
open import PropertyRegistry as P

-- Lightweight expression placeholder (for textual equations / witnesses)
RelExpr : Set
RelExpr = String

------------------------------------------------------------------------
-- Section 3.1: The calculus of relations (in a regular category)
------------------------------------------------------------------------

-- A relation R ⊆ A × B represented by a mono m_R : R_obj → A×B
record RelationDeclaration : Set where
  constructor RELATION_from_to
  field
    category         : M.Identifier
    A                : M.Identifier
    B                : M.Identifier
    relation         : M.Identifier         -- name/id for the relation R
    relationObject   : M.Identifier         -- R_obj
    monoToProduct    : M.Identifier         -- m_R : R_obj → A×B
-- CATEGORY: Subobject of A×B in a regular category.

-- Composition R∘S via pullback then image
record RelationComposition : Set where
  constructor _∘_
  field
    S                  : RelationDeclaration -- S : A ⇸ B
    R                  : RelationDeclaration -- R : B ⇸ C
    pullbackObject     : M.Identifier        -- P_obj
    map_to_A           : M.Identifier        -- P_obj → A
    map_to_C           : M.Identifier        -- P_obj → C
    composite_map      : M.Identifier        -- P_obj → A×C
    imageMono          : M.Identifier        -- mono representing Image(composite_map)
-- CATEGORY: Pullback-then-image construction of relational composition.

-- Opposite (converse) relation R^op : B ⇸ A
record OppositeRelation : Set where
  constructor _^op
  field
    original          : RelationDeclaration
    swapIsomorphism   : M.Identifier   -- τ : A×B → B×A
    oppositeObject    : M.Identifier   -- R^op_obj
    oppositeMono      : M.Identifier   -- mono for Image(τ ∘ m_R)
-- CATEGORY: Swap the components via τ and take image.

-- Identity relation on A from the diagonal Δ : A → A×A
record IdentityRelation : Set where
  constructor Id_Rel
  field
    category : M.Identifier
    A        : M.Identifier
    diagonal : M.Identifier -- Δ : A → A×A (mono)
-- CATEGORY: Identity for relational composition.

-- The category Rel(C): same objects as C; morphisms are relations
record CategoryOfRelations : Set where
  constructor Rel
  field
    baseCategory    : M.Identifier   -- C (assumed regular)
    relCategory     : M.Identifier   -- Rel(C)
    compositionOp   : M.Identifier   -- reference to RelationComposition
    identityFamily  : M.Identifier   -- reference to Id_Rel constructor family
-- CATEGORY: Category whose morphisms are subobjects of products.

------------------------------------------------------------------------
-- Sections 3.2–3.4: Lawvere theories, models, and categories of models
------------------------------------------------------------------------

-- Lawvere theory: category with finite products; every object ≅ X^n
record LawvereTheoryDeclaration : Set where
  constructor LAWVERE_THEORY_WITH_base_object
  field
    theoryCategory          : M.Identifier -- T
    baseObject              : M.Identifier -- X in T
    finiteProductsWitness   : M.Identifier
    arityDecompositionProof : M.Identifier -- ∀Obj ≅ X^n
-- CATEGORY: Syntactic category encoding operations by morphisms.

-- Operation in a Lawvere theory: X^n → X^m
record OperationInTheory : Set where
  constructor OPERATION_COLON
  field
    name        : M.Identifier
    theory      : LawvereTheoryDeclaration
    arity_n     : String
    arity_m     : String
    morphismId  : M.Identifier -- the morphism in T
-- CATEGORY: Tuple of n-ary operations (m outputs) as a morphism in T.

-- Axiom in a Lawvere theory: a commuting diagram (f == g)
record AxiomInTheory : Set where
  constructor AXIOM_Diagram_COMMUTES
  field
    theory   : LawvereTheoryDeclaration
    name     : String
    diagram  : M.Identifier  -- identifies the diagram
    lhs      : M.Identifier  -- f
    rhs      : M.Identifier  -- g
-- CATEGORY: Equational law encoded diagrammatically.

-- Model: product-preserving functor T → C
record ModelOfTheory : Set where
  constructor MODEL_of_within
  field
    theory    : LawvereTheoryDeclaration
    targetCat : M.Identifier -- C (with finite products)
    functorId : M.Identifier -- M : T → C
    preservesProductsWitness : M.Identifier
-- CATEGORY: Semantic interpretation of T in C.

-- Category of models Mod(T, C)
record CategoryOfModels : Set where
  constructor Mod_of_within
  field
    theory         : LawvereTheoryDeclaration
    targetCat      : M.Identifier
    modelsCategory : M.Identifier -- Mod(T,C)
-- CATEGORY: Objects are models, morphisms are natural transformations.

------------------------------------------------------------------------
-- Sections 3.5–3.7: Properties of algebraic categories
------------------------------------------------------------------------

-- Declaration that a category is algebraic (equivalent to Mod(T,Set))
record AlgebraicCategoryDeclaration : Set where
  constructor _is_ALGEBRAIC_CATEGORY
  field
    category     : M.Identifier
    witnessTheory : LawvereTheoryDeclaration
    equivalenceWitness : M.Identifier -- category ≅ Mod(T,Set)

-- Theorem: Every algebraic category is complete and cocomplete
record AlgebraicCategoriesAreCompleteAndCocompleteTheorem : Set where
  constructor THEOREM_AlgebraicCategoriesAreCompleteAndCocomplete
  field
    category          : AlgebraicCategoryDeclaration
    completeWitness   : M.Identifier
    cocompleteWitness : M.Identifier

-- Theorem: Every algebraic category is regular
record AlgebraicCategoriesAreRegularTheorem : Set where
  constructor THEOREM_AlgebraicCategoriesAreRegular
  field
    category      : AlgebraicCategoryDeclaration
    regularWitness : M.Identifier

-- Forgetful functor U : C → Set for an algebraic category C
record ForgetfulFunctor_Algebraic : Set where
  constructor U_OF
  field
    C          : AlgebraicCategoryDeclaration
    functorId  : M.Identifier -- U_C

-- Free functor F : Set → C for an algebraic category C
record FreeFunctor_Algebraic : Set where
  constructor F_TO
  field
    C          : AlgebraicCategoryDeclaration
    functorId  : M.Identifier -- F

-- Theorem: Free-forgetful adjunction (F ⊣ U)
record ExistenceOfFreeFunctorAdjunctionTheorem : Set where
  constructor THEOREM_ExistenceOfFreeFunctorAdjunction
  field
    C                  : AlgebraicCategoryDeclaration
    freeFunctor        : FreeFunctor_Algebraic
    forgetfulFunctor   : ForgetfulFunctor_Algebraic
    adjunctionWitness  : M.Identifier -- F ⊣ U

-- Lattice-theoretic: element c is compact in a complete lattice L
record CompactElementProperty : Set where
  constructor _is_COMPACT_IN_
  field
    element : M.Identifier
    lattice : M.Identifier
    compactnessWitness : M.Identifier

-- Lattice property: L is an algebraic lattice
record AlgebraicLatticeProperty : Set where
  constructor _is_ALGEBRAIC_LATTICE
  field
    lattice : M.Identifier
    algebraicWitness : M.Identifier

-- Theorem: Sub(X) is algebraic in any algebraic category C
record SubobjectLatticesInAlgebraicCategoriesAreAlgebraicTheorem : Set where
  constructor THEOREM_SubobjectLatticesInAlgebraicCategoriesAreAlgebraic
  field
    C                 : AlgebraicCategoryDeclaration
    objectX           : M.Identifier
    subobjectLattice  : M.Identifier -- Sub(X)
    algebraicWitness  : M.Identifier

------------------------------------------------------------------------
-- Section 3.8: Algebraic functors and characterization
------------------------------------------------------------------------

-- Forgetful functor of an algebraic category (notation U_C)
record ForgetfulFunctorOfAlgebraic : Set where
  constructor U_of_
  field
    C         : AlgebraicCategoryDeclaration
    functorId : M.Identifier -- U_C : C → Set

-- Algebraic functor F : C → D commuting with forgetfuls up to iso
record AlgebraicFunctorDeclaration : Set where
  constructor _is_ALGEBRAIC_FUNCTOR
  field
    F                : M.Identifier
    source           : AlgebraicCategoryDeclaration
    target           : AlgebraicCategoryDeclaration
    commutingIso     : M.Identifier -- α : U_D ∘ F ⇒ U_C

-- Product-preserving morphism of theories φ : T1 → T2
record LawvereTheoryMorphism : Set where
  constructor THEORY_MORPHISM_COLON
  field
    phiId                  : M.Identifier
    sourceTheory           : LawvereTheoryDeclaration
    targetTheory           : LawvereTheoryDeclaration
    preservesProductsProof : M.Identifier

-- Induced functor on models Mod(φ) : Mod(T2,Set) → Mod(T1,Set)
record InducedFunctorOnModels : Set where
  constructor Mod_OF
  field
    phi           : LawvereTheoryMorphism
    inducedFunctor : M.Identifier

-- Characterization theorem: algebraic functors arise from theory morphisms
record CharacterizationOfAlgebraicFunctorsTheorem : Set where
  constructor THEOREM_CharacterizationOfAlgebraicFunctors
  field
    F                      : AlgebraicFunctorDeclaration
    underlyingPhi          : LawvereTheoryMorphism
    naturalIsoWitness      : M.Identifier -- F ≅ Mod(φ)

------------------------------------------------------------------------
-- Section 3.11: Commutative theories (interchange law) and consequences
------------------------------------------------------------------------

-- Interchange axiom for two operations in a theory
record CommutativityAxiom : Set where
  constructor AXIOM_Interchange_for_
  field
    theory : LawvereTheoryDeclaration
    fOp    : M.Identifier -- f : X^n → X
    gOp    : M.Identifier -- g : X^m → X
    interchangeWitness : M.Identifier -- Path1 == Path2

-- Commutative Lawvere theory: every pair of ops satisfies interchange
record CommutativeLawvereTheory : Set where
  constructor COMMUTATIVE_
  field
    theory                 : LawvereTheoryDeclaration
    globalInterchangeProof : M.Identifier

-- Theorem: Mod(T,Set) is symmetric monoidal when T is commutative
record CommutativeTheoriesYieldMonoidalCategoriesTheorem : Set where
  constructor THEOREM_CommutativeTheoriesYieldMonoidalCategories
  field
    T                      : CommutativeLawvereTheory
    modelsCategory         : M.Identifier -- Mod(T,Set)
    monoidalStructureProof : M.Identifier -- Mod(T,Set) symmetric monoidal

-- Instances
record TheoryOfRModulesIsCommutativeInstance : Set where
  constructor INSTANCE_TheoryOfRModulesIsCommutative
  field
    theoryRMod : LawvereTheoryDeclaration
    proof      : M.Identifier

record TheoryOfGroupsIsNotCommutativeInstance : Set where
  constructor INSTANCE_TheoryOfGroupsIsNotCommutative
  field
    theoryGrp : LawvereTheoryDeclaration
    equivalenceWitness : M.Identifier -- (commutative iff models abelian)

------------------------------------------------------------------------
-- Section 3.12: Tensor product of theories and bialgebras
------------------------------------------------------------------------

-- (T₁,T₂)-bialgebra in Set
record BialgebraDeclaration : Set where
  constructor _is_T1_T2_Bialgebra
  field
    T1        : LawvereTheoryDeclaration
    T2        : LawvereTheoryDeclaration
    carrier   : M.Identifier -- set S
    model1    : M.Identifier -- M₁ : T₁ → Set with carrier S
    model2    : M.Identifier -- M₂ : T₂ → Set with carrier S
    compatibilityWitness : M.Identifier -- interchange for all (op1,op2)

-- Category of bialgebras Bialg(T1,T2)
record CategoryOfBialgebras : Set where
  constructor Bialg_of_
  field
    T1        : LawvereTheoryDeclaration
    T2        : LawvereTheoryDeclaration
    category  : M.Identifier -- Bialg(T1,T2)

-- Tensor product of theories T1 ⊗ T2
record TensorProductOfTheories : Set where
  constructor _⊗_
  field
    T1                  : LawvereTheoryDeclaration
    T2                  : LawvereTheoryDeclaration
    tensorTheory        : M.Identifier -- T1 ⊗ T2
    constructionWitness : M.Identifier -- ops/axioms + interchange

-- Theorem: Mod(T1 ⊗ T2, Set) ≅ Bialg(T1,T2)
record TensorProductModelsAreBialgebrasTheorem : Set where
  constructor THEOREM_TensorProductModelsAreBialgebras
  field
    T1        : LawvereTheoryDeclaration
    T2        : LawvereTheoryDeclaration
    equivalenceWitness : M.Identifier -- equivalence of categories

-- Concrete instance: Rings as tensor of Monoids and AbGroups
record TheoryOfRingsAsTensorProductInstance : Set where
  constructor INSTANCE_TheoryOfRingsAsTensorProduct
  field
    T_Mon     : LawvereTheoryDeclaration
    T_Ab      : LawvereTheoryDeclaration
    T_RingIso : M.Identifier            -- TheoryOfRings ≅ T_Mon ⊗ T_Ab
    categoryEquivWitness : M.Identifier -- Mod ≅ Bialg

------------------------------------------------------------------------
-- Section 3.13: Morita equivalence
------------------------------------------------------------------------

-- Morita equivalence of theories (semantic equivalence of model cats)
record MoritaEquivalence : Set where
  constructor _is_MORITA_EQUIVALENT_TO_
  field
    T1 : LawvereTheoryDeclaration
    T2 : LawvereTheoryDeclaration
    equivalenceWitness : M.Identifier -- Mod(T1,Set) ≅ Mod(T2,Set)

-- Progenerator module over a ring R (in R-Mod)
record ProgeneratorModule : Set where
  constructor _is_PROGENERATOR_MODULE_over_
  field
    ringCategory : M.Identifier  -- R-Mod
    moduleP      : M.Identifier
    finitelyGeneratedWitness : M.Identifier
    projectiveWitness        : M.Identifier
    generatorWitness         : M.Identifier

-- Functors built from a bimodule P: - ⊗_R P and Hom_S(P,-)
record MoritaEquivalenceFunctor_Tensor : Set where
  constructor _⊗_R_
  field
    R        : M.Identifier
    P        : M.Identifier -- (S,R)-bimodule identifier
    functor  : M.Identifier -- - ⊗_R P : R-Mod → S-Mod

record MoritaEquivalenceFunctor_Hom : Set where
  constructor Hom_FUN
  field
    S        : M.Identifier
    P        : M.Identifier -- (S,R)-bimodule identifier
    functor  : M.Identifier -- Hom_S(P,-) : S-Mod → R-Mod

-- Morita theorem for rings
record MoritaTheoremForRings : Set where
  constructor THEOREM_MoritaTheoremForRings
  field
    R                : M.Identifier
    S                : M.Identifier
    progenerator     : ProgeneratorModule
    endomorphismRingIso : M.Identifier -- End_R(P) ≅ S
    equivalenceWitness   : M.Identifier -- R-Mod ≃ S-Mod

-- Canonical instance: R ≃ Mₙ(R)
record MoritaEquivalenceOfMatrixRingsInstance : Set where
  constructor INSTANCE_MoritaEquivalenceOfMatrixRings
  field
    R                : M.Identifier
    n                : String
    matrixRing       : M.Identifier -- Mₙ(R)
    progenerator     : ProgeneratorModule
    endoRingIso      : M.Identifier -- End(Rⁿ) ≅ Mₙ(R)
    equivalenceProof : M.Identifier -- module category equivalence

------------------------------------------------------------------------
-- Bridge postulates placeholder (integration with unified proof layer)
------------------------------------------------------------------------

-- Note: This section intentionally refrains from introducing new Level 1
-- subjects/axiom names. When desirable, specific results from this file can
-- be bridged into the generic property scaffolding (e.g., CategoryHasPropertyS)
-- with stable identifiers added to PropertyRegistry, following the established
-- canonicalization pattern.

------------------------------------------------------------------------
-- Section 3.10: Beck's Characterization (internal recognition of algebraic)
------------------------------------------------------------------------

-- Object-level property: an object P is regular projective
record RegularProjectiveObjectProperty : Set where
  constructor _is_REGULAR_PROJECTIVE
  field
    category : M.Identifier
    objectP  : M.Identifier
    liftingSurjectivityWitness : M.Identifier -- surjectivity of Hom(P, -) along reg epis

-- Coproduct_over(I) of G
record CoproductOfGenerator : Set where
  constructor Coproduct_over_of
  field
    category : M.Identifier
    indexSet : M.Identifier
    generator : M.Identifier
    coproductObject : M.Identifier

-- Category property: C is REGULARLY_COVERED_BY G
record RegularlyCoveredByGeneratorProperty : Set where
  constructor _is_REGULARLY_COVERED_BY_
  field
    category  : M.Identifier
    generator : M.Identifier
    coveringWitness : M.Identifier -- every X receives a reg epi from a coproduct of G

-- Beck characterization theorem
record BeckCharacterizationOfAlgebraicCategoriesTheorem : Set where
  constructor THEOREM_BeckCharacterizationOfAlgebraicCategories
  field
    category           : M.Identifier
    cocompleteWitness  : M.Identifier
    generator          : M.Identifier
    generatorIsRegularProjective : RegularProjectiveObjectProperty
    generatorIsGeneratorWitness  : M.Identifier -- G is GENERATOR
    regularlyCoveredByG : RegularlyCoveredByGeneratorProperty
    equivalenceWitness  : M.Identifier -- ALGEBRAIC_CATEGORY <==> (conditions)

postulate
  -- Algebraic category as a generic property
  algebraicCategoryBridge
    : (decl : AlgebraicCategoryDeclaration)
    -> C.Proof (C.CategoryHasPropertyS (AlgebraicCategoryDeclaration.category decl)
                                       P.AlgebraicCategoryId)
               C.HasPropertyName

  -- Algebraic categories are complete and cocomplete
  algebraicIsCompleteBridge
    : (thm : AlgebraicCategoriesAreCompleteAndCocompleteTheorem)
    -> C.Proof (C.CategoryHasPropertyS (AlgebraicCategoryDeclaration.category
                                         (AlgebraicCategoriesAreCompleteAndCocompleteTheorem.category thm))
                                       P.CompleteCategoryId)
               C.HasPropertyName

  algebraicIsCocompleteBridge
    : (thm : AlgebraicCategoriesAreCompleteAndCocompleteTheorem)
    -> C.Proof (C.CategoryHasPropertyS (AlgebraicCategoryDeclaration.category
                                         (AlgebraicCategoriesAreCompleteAndCocompleteTheorem.category thm))
                                       P.CocompleteCategoryId)
               C.HasPropertyName

  -- Algebraic categories are regular
  algebraicIsRegularBridge
    : (thm : AlgebraicCategoriesAreRegularTheorem)
    -> C.Proof (C.CategoryHasPropertyS (AlgebraicCategoryDeclaration.category
                                         (AlgebraicCategoriesAreRegularTheorem.category thm))
                                       P.RegularCategoryId)
               C.HasPropertyName

  -- Models of commutative theories are symmetric monoidal
  modelsOfCommutativeTheoryAreSymmetricMonoidalBridge
    : (thm : CommutativeTheoriesYieldMonoidalCategoriesTheorem)
    -> C.Proof (C.CategoryHasPropertyS (CommutativeTheoriesYieldMonoidalCategoriesTheorem.modelsCategory thm)
                                       P.SymmetricMonoidalCategoryId)
               C.HasPropertyName

  -- Existence of free-forgetful adjunction in an algebraic category
  freeForgetfulAdjunctionBridge
    : (thm : ExistenceOfFreeFunctorAdjunctionTheorem)
    -> C.Proof (C.CategoryHasPropertyS (AlgebraicCategoryDeclaration.category (ExistenceOfFreeFunctorAdjunctionTheorem.C thm))
                                      P.HasFreeForgetfulAdjunctionId)
               C.HasPropertyName

  -- Algebraic lattice properties (generic, can be applied to Sub(X))
  algebraicLatticeBridge
    : (prop : AlgebraicLatticeProperty)
    -> C.Proof (C.CategoryHasPropertyS (AlgebraicLatticeProperty.lattice prop)
                                      P.AlgebraicLatticeId)
               C.HasPropertyName

  subobjectLatticeAlgebraicBridge
    : (thm : SubobjectLatticesInAlgebraicCategoriesAreAlgebraicTheorem)
    -> C.Proof (C.CategoryHasPropertyS (SubobjectLatticesInAlgebraicCategoriesAreAlgebraicTheorem.subobjectLattice thm)
                                      P.AlgebraicLatticeId)
               C.HasPropertyName

  -- Beck characterization: category-level generic properties
  hasRegularProjectiveGeneratorBridge
    : (thm : BeckCharacterizationOfAlgebraicCategoriesTheorem)
    -> C.Proof (C.CategoryHasPropertyS (BeckCharacterizationOfAlgebraicCategoriesTheorem.category thm)
                                      P.HasRegularProjectiveGeneratorId)
               C.HasPropertyName

  regularlyCoveredByGeneratorBridge
    : (prop : RegularlyCoveredByGeneratorProperty)
    -> C.Proof (C.CategoryHasPropertyS (RegularlyCoveredByGeneratorProperty.category prop)
                                      P.RegularlyCoveredByGeneratorId)
               C.HasPropertyName

  -- Category has a generator (extracted from Beck characterization data)
  hasGeneratorBridge
    : (thm : BeckCharacterizationOfAlgebraicCategoriesTheorem)
    -> C.Proof (C.CategoryHasPropertyS (BeckCharacterizationOfAlgebraicCategoriesTheorem.category thm)
                                      P.HasGeneratorId)
               C.HasPropertyName

  -- Algebraic functor as a generic functor-level property
  algebraicFunctorBridge
    : (decl : AlgebraicFunctorDeclaration)
    -> C.Proof (C.FunctorHasPropertyS (AlgebraicFunctorDeclaration.F decl)
                                      (AlgebraicCategoryDeclaration.category (AlgebraicFunctorDeclaration.source decl))
                                      (AlgebraicCategoryDeclaration.category (AlgebraicFunctorDeclaration.target decl))
                                      P.AlgebraicFunctorId)
               C.HasPropertyName
