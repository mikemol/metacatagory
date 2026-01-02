{-# OPTIONS --without-K #-}

-- | Chapter 2 §2: regular/exact category scaffolding—finite limits, regular
--   epis, pullback stability, and factorization-system witnesses.
module Chapter2.Level2sub2 where

open import Agda.Builtin.Unit     using (⊤; tt)
open import Agda.Builtin.String   using (String)
open import Agda.Builtin.List     using (List; []; _∷_)
open import Core.Phase using (Bool)
open import Agda.Builtin.Equality using (_≡_)

open import Metamodel as M
open import Core      as C

-- Placeholder for more structured expressions
RegExpr : Set
RegExpr = String

------------------------------------------------------------------------
-- Section 2.1 & 2.2: Regular Categories (Finite limits, regular epis, stability)
------------------------------------------------------------------------

-- Property: C has FINITE_LIMITS
record FiniteLimitsProperty : Set where
  constructor _has_FINITE_LIMITS
  field
    category : M.Identifier
    hasTerminalWitness : M.Identifier
    hasPullbacksWitness : M.Identifier
-- CATEGORY: Terminal object + all pullbacks.

-- Morphism property: e is REGULAR_EPIMORPHISM
record RegularEpimorphismProperty : Set where
  constructor _is_REGULAR_EPIMORPHISM
  field
    morphism : M.Identifier
    domain   : M.Identifier
    codomain : M.Identifier
    presentingPairSource : M.Identifier -- X in (f,g : X -> domain)
    parallelPair_f : M.Identifier       -- f : X -> A
    parallelPair_g : M.Identifier       -- g : X -> A
    coequalizerWitness : M.Identifier   -- coequalizer(f,g) ≅ morphism
-- CATEGORY: Arises as a coequalizer of a parallel pair.

-- Class stability under pullback: E is STABLE_UNDER_PULLBACK
record StabilityUnderPullbackProperty : Set where
  constructor _is_STABLE_UNDER_PULLBACK
  field
    classId : M.Identifier            -- identifier for class E
    pullbackClosureWitness : M.Identifier -- proof that all pullbacks stay in E
-- CATEGORY: Pullback of a member remains in the class.

-- Regular category declaration
record RegularCategoryDeclaration : Set where
  constructor REGULAR_CATEGORY
  field
    category : M.Identifier
    finiteLimits : FiniteLimitsProperty
    regularEpiMonoFactorizationWitness : M.Identifier -- FACTORIZATION_SYSTEM(RegularEpi, Mono)
    regularEpiStability : StabilityUnderPullbackProperty
-- CATEGORY: Finite limits + (RegEpi, Mono) factorization + stability.

-- Theorem: RegularEpisAreStrong
record RegularEpisAreStrongTheorem : Set where
  constructor THEOREM_RegularEpisAreStrong
  field
    category : M.Identifier
    epi      : RegularEpimorphismProperty
    strongEpiWitness : M.Identifier
-- CATEGORY: Regular epi implies strong epi.

------------------------------------------------------------------------
-- Section 2.3 & 2.5: Kernel pairs, internal equivalence relations, effectiveness
------------------------------------------------------------------------

-- Kernel pair of f
record KernelPairDeclaration : Set where
  constructor KernelPair_of
  field
    morphism : M.Identifier -- f : A -> B
    objectK  : M.Identifier -- K
    projection1 : M.Identifier -- k1 : K -> A
    projection2 : M.Identifier -- k2 : K -> A
    pullbackSquareWitness : M.Identifier
-- CATEGORY: Pullback(f,f) with projections.

-- Internal equivalence relation on A given (r1,r2)
record InternalEquivalenceRelationDeclaration : Set where
  constructor INTERNAL_EQUIV_RELATION_on
  field
    objectR : M.Identifier
    objectA : M.Identifier
    relLeft : M.Identifier -- r1:R->A
    relRight : M.Identifier -- r2:R->A
    monoIntoProductWitness : M.Identifier -- <r1,r2> mono
    reflexivityWitness  : M.Identifier
    symmetryWitness     : M.Identifier
    transitivityWitness : M.Identifier
-- CATEGORY: Encodes internal reflexive, symmetric, transitive relation.

-- Regular exact sequence K --(k1,k2)--> A --q--> Q
record RegularExactSequenceDeclaration : Set where
  constructor REGULAR_EXACT_SEQUENCE
  field
    kernelPair : KernelPairDeclaration
    quotient   : RegularEpimorphismProperty -- q : A -> Q (regular epi)
    compatibilityWitness : M.Identifier -- kernelPair ≅ KernelPair(q)
-- CATEGORY: Fundamental unit of exactness in regular setting.

-- Theorem: KernelPairIsEquivalenceRelation
record KernelPairIsEquivalenceRelationTheorem : Set where
  constructor THEOREM_KernelPairIsEquivalenceRelation
  field
    category : M.Identifier
    kernelPair : KernelPairDeclaration
    equivalenceRelationWitness : InternalEquivalenceRelationDeclaration
-- CATEGORY: Kernel pair always forms an internal equivalence relation.

-- Theorem: RegularCategoriesHaveEffectiveRelations
record RegularCategoriesHaveEffectiveRelationsTheorem : Set where
  constructor THEOREM_RegularCategoriesHaveEffectiveRelations
  field
    category : M.Identifier
    isRegular : RegularCategoryDeclaration
    effectivenessWitness : M.Identifier -- ∀R, relation effective
    equivalenceStatementWitness : M.Identifier -- (P1 <==> P2)
-- CATEGORY: Every internal equivalence relation is effective.

-- Exact category: Regular + effective equivalence relations
record ExactCategoryDeclaration : Set where
  constructor EXACT_CATEGORY
  field
    category : M.Identifier
    regular : RegularCategoryDeclaration
    effectiveRelationsTheorem : RegularCategoriesHaveEffectiveRelationsTheorem
-- CATEGORY: Regular category where all internal equivalence relations are effective.

------------------------------------------------------------------------
-- Section 2.6: Barr-exact categories
------------------------------------------------------------------------

-- Class closure under composition: E is CLOSED_UNDER_COMPOSITION
record ClosedUnderCompositionProperty : Set where
  constructor _is_CLOSED_UNDER_COMPOSITION
  field
    classId : M.Identifier
    compositionClosureWitness : M.Identifier
-- CATEGORY: Quotients compose.

-- Barr-exact category declaration
record BarrExactCategoryDeclaration : Set where
  constructor BARR_EXACT_CATEGORY
  field
    category : M.Identifier
    regular   : RegularCategoryDeclaration
    regularEpiCompositionClosure : ClosedUnderCompositionProperty
-- CATEGORY: Regular + regular epis closed under composition.

-- Barr-exact functor F is BARR_EXACT_FUNCTOR
record BarrExactFunctorDeclaration : Set where
  constructor _is_BARR_EXACT_FUNCTOR
  field
    functorId : M.Identifier
    sourceCat : M.Identifier
    targetCat : M.Identifier
    preservesFiniteLimitsWitness : M.Identifier
    preservesRegularEpisWitness  : M.Identifier
-- CATEGORY: Preserves finite limits and regular epis.

------------------------------------------------------------------------
-- Section 2.7: Barr's Embedding Theorem
------------------------------------------------------------------------

-- BarrEmbedding theorem statement
record BarrEmbeddingTheoremDeclaration : Set where
  constructor THEOREM_BarrEmbedding
  field
    category : M.Identifier
    smallWitness : M.Identifier
    targetIndexCategory : M.Identifier -- K
    embeddingFunctor : M.Identifier    -- F : C -> Psh(K)
    fullyFaithfulWitness : M.Identifier
    exactFunctorWitness : M.Identifier -- F is Barr-exact functor
-- CATEGORY: Existence of full faithful exact embedding.

-- Element-like reasoning justification axiom
record ElementLikeReasoningJustificationAxiom : Set where
  constructor JUSTIFICATION_AXIOM_GeneralizedElementReasoning_is_SOUND_within_ExactCats
  field
    barrEmbeddingTheoremWitness : BarrEmbeddingTheoremDeclaration
    soundnessPropositionWitness : M.Identifier
-- CATEGORY: Embedding justifies element-based reasoning.

------------------------------------------------------------------------
-- Bridge postulates (placeholders; proof layer extension for regular/exact to follow)
------------------------------------------------------------------------

postulate
  regularCategoryBridge
    : (decl : RegularCategoryDeclaration)
    -> C.Proof (C.RegularCategoryS (RegularCategoryDeclaration.category decl)) C.RegularCategoryName

  barrExactCategoryBridge
    : (decl : BarrExactCategoryDeclaration)
    -> C.Proof (C.BarrExactCategoryS (BarrExactCategoryDeclaration.category decl)) C.BarrExactCategoryName

  regularEpisAreStrongBridge
    : (thm : RegularEpisAreStrongTheorem)
    -> C.Proof (C.RegularEpisAreStrongS (RegularEpisAreStrongTheorem.category thm) (RegularEpimorphismProperty.morphism (RegularEpisAreStrongTheorem.epi thm))) C.RegularEpisAreStrongName

  kernelPairEquivRelBridge
    : (thm : KernelPairIsEquivalenceRelationTheorem)
    -> C.Proof
         (C.KernelPairEquivRelS (KernelPairIsEquivalenceRelationTheorem.category thm)
                                (KernelPairDeclaration.morphism (KernelPairIsEquivalenceRelationTheorem.kernelPair thm))
                                (KernelPairDeclaration.objectK (KernelPairIsEquivalenceRelationTheorem.kernelPair thm))
                                (KernelPairDeclaration.projection1 (KernelPairIsEquivalenceRelationTheorem.kernelPair thm))
                                (KernelPairDeclaration.projection2 (KernelPairIsEquivalenceRelationTheorem.kernelPair thm)))
         C.KernelPairEquivRelName

  effectiveRelationsBridge
    : (thm : RegularCategoriesHaveEffectiveRelationsTheorem)
    -> C.Proof (C.EffectiveRelationsS (RegularCategoriesHaveEffectiveRelationsTheorem.category thm)) C.EffectiveRelationsName

  exactCategoryBridge
    : (decl : ExactCategoryDeclaration)
    -> C.Proof (C.ExactCategoryS (ExactCategoryDeclaration.category decl)) C.ExactCategoryName

  barrExactFunctorPreservesFiniteLimitsBridge
    : (decl : BarrExactFunctorDeclaration)
    -> C.Proof (C.PreservesFiniteLimitsS (BarrExactFunctorDeclaration.functorId decl)
                                         (BarrExactFunctorDeclaration.sourceCat decl)
                                         (BarrExactFunctorDeclaration.targetCat decl))
               C.PreservesFiniteLimitsName

  barrExactFunctorPreservesRegularEpisBridge
    : (decl : BarrExactFunctorDeclaration)
    -> C.Proof (C.PreservesRegularEpisS (BarrExactFunctorDeclaration.functorId decl)
                                        (BarrExactFunctorDeclaration.sourceCat decl)
                                        (BarrExactFunctorDeclaration.targetCat decl))
               C.PreservesRegularEpisName

  barrEmbeddingBridge
    : (thm : BarrEmbeddingTheoremDeclaration)
    -> C.Proof (C.BarrEmbeddingS (BarrEmbeddingTheoremDeclaration.category thm)
                                 (BarrEmbeddingTheoremDeclaration.targetIndexCategory thm)
                                 (BarrEmbeddingTheoremDeclaration.embeddingFunctor thm))
              C.BarrEmbeddingTheoremName

------------------------------------------------------------------------
-- End of structural encoding for Regular & Barr-exact categories
------------------------------------------------------------------------
