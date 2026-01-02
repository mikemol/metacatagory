{-# OPTIONS --without-K #-}

-- | Chapter 1 §4: subobject calculus—equivalence of monos, lattices,
--   (co)well-poweredness, and lattice operations such as intersection/union.
module Chapter1.Level1sub4 where

open import Agda.Builtin.Unit     using (⊤; tt)
open import Agda.Builtin.String   using (String)
open import Agda.Builtin.List     using (List; []; _∷_)
open import Core.Phase using (Bool; true; false)
open import Agda.Builtin.Equality using (_≡_)

open import Metamodel as M
open import Core      as C

-- Lightweight expression placeholder
SubExpr : Set
SubExpr = String

------------------------------------------------------------------------
-- Part 1: Subobjects and equivalence relation
------------------------------------------------------------------------

record MonomorphismEquivalence : Set where
  constructor _≈_Sub_
  field m1 m2 X : M.Identifier
-- CATEGORY: Equivalence relation on monomorphisms into X.

record SubobjectAsEquivalenceClass : Set where
  constructor [_]_X
  field m X : M.Identifier
-- CATEGORY: Subobject as equivalence class [m]_X.

record SubobjectOrdering : Set where
  constructor _≤_
  field s1 s2 : M.Identifier
-- CATEGORY: Ordering on subobjects.

record SubobjectLattice : Set where
  constructor Sub
  field X : M.Identifier
-- CATEGORY: Poset of subobjects of X.

record WellPoweredCategory : Set where
  constructor _is_WELL_POWERED
  field C : M.Identifier
-- CATEGORY: Every object has only a set of subobjects.

record InferDualTheory_WellCopowered : Set where
  constructor INFER_DUAL_THEORY_Well_Copowered
  field unit : ⊤
-- CATEGORY: Dual of well-powered derived via duality.

------------------------------------------------------------------------
-- Part 2: Intersection and union (lattice operations)
------------------------------------------------------------------------

record WidePullbackDiagram : Set where
  constructor DIAGRAM_for_Intersection
  field subobjects : List M.Identifier

-- | Construct intersection via wide pullback.
record IntersectionConstructor : Set where
  constructor LIMIT_OF
  field diagram : WidePullbackDiagram

-- | Binary intersection of subobjects.
record BinaryIntersection : Set where
  constructor _∩_
  field s1 s2 : M.Identifier

-- | Construct union via image of coproduct map.
record UnionConstructor : Set where
  constructor Image_of_UniversalCoproductMap
  field subobjects : List M.Identifier

-- | Binary union of subobjects.
record BinaryUnion : Set where
  constructor _∪_
  field s1 s2 : M.Identifier

-- | Property: poset P is a complete lattice.
record LatticePropertyDefinition : Set where
  constructor _is_COMPLETE_LATTICE
  field P : M.Identifier

-- | Theorem: subobject lattice is complete.
record SubobjectLatticeIsComplete : Set where
  constructor THEOREM_SubobjectLatticeIsComplete
  field unit : ⊤

------------------------------------------------------------------------
-- Part 3: Strong epimorphisms and orthogonality
------------------------------------------------------------------------

-- | Square used to test orthogonality (epi/mono).
record CommutativeSquareForOrthogonalityTest : Set where
  constructor ORTHOGONALITY_SQUARE_VIA
  field e m f g : M.Identifier

-- | Unique diagonal filler property.
record DiagonalFillerProperty : Set where
  constructor HasUniqueDiagonalFiller
  field sq : M.Identifier

-- | Strong epimorphism declaration.
record StrongEpimorphism : Set where
  constructor _is_STRONG_EPIMORPHISM
  field e : M.Identifier

-- | Theorem: canonical (strong epi, mono) factorization system.
record CanonicalFactorizationSystem : Set where
  constructor THEOREM_CanonicalFactorizationSystem
  field unit : ⊤

-- | Strong monomorphism declaration.
record StrongMonomorphism : Set where
  constructor _is_STRONG_MONOMORPHISM
  field m : M.Identifier

-- | Dual factorization system theorem (via duality).
record DualFactorizationSystemTheorem : Set where
  constructor INFER_DUAL_THEOREM_DualFactorizationSystem
  field unit : ⊤

------------------------------------------------------------------------
-- Part 4: Epi-mono factorizations
------------------------------------------------------------------------

-- | Factorization data f = m ∘ e via an object I.
record MorphismFactorization : Set where
  constructor Factorization_of_is_via
  field f e m I : M.Identifier

-- | Axiom: morphism factorizations are unique up to isomorphism.
record FactorizationUniquenessAxiom : Set where
  constructor AXIOM_FactorizationIsUniqueUpToIsomorphism
  field unit : ⊤

-- | Declaration that C admits a chosen (E,M) factorization system.
record HasFactorizationSystem : Set where
  constructor _has_FACTORIZATION_SYSTEM
  field C E M : M.Identifier

-- | Image of a morphism f.
record ImageOfMorphism : Set where
  constructor Image
  field f : M.Identifier

-- | Coimage of a morphism f.
record CoimageOfMorphism : Set where
  constructor Coimage
  field f : M.Identifier

-- | Standard epi–mono factorization system for a category C.
record StandardFactorizationSystem : Set where
  constructor THEOREM_StandardFactorizationSystem
  field C : M.Identifier

-- | Dual factorization system (monic–epic).
record AlternateFactorizationSystem : Set where
  constructor INFER_DUAL_THEOREM_AlternateFactorizationSystem
  field unit : ⊤

------------------------------------------------------------------------
-- Part 5: Generators
------------------------------------------------------------------------

record HasGeneratorObject : Set where
  constructor _has_GENERATOR_
  field C G : M.Identifier
-- CATEGORY: G is a generator if Hom(G,-) is faithful.

------------------------------------------------------------------------
-- Part 6: Projectives
------------------------------------------------------------------------

-- | Lifting problem against an epimorphism e.
record ProjectiveLiftingProblem : Set where
  constructor LIFTING_PROBLEM_against_epi
  field f e : M.Identifier

-- | Existence of a lift for a given problem.
record HasLiftSolution : Set where
  constructor HAS_LIFT
  field problem : M.Identifier

-- | Projective object declaration.
record ProjectiveObject : Set where
  constructor _is_PROJECTIVE
  field P : M.Identifier

-- | Functorial equivalence characterization of projectives.
record ProjectiveFunctorialEquivalence : Set where
  constructor THEOREM_ProjectiveFunctorialEquivalence
  field P : M.Identifier

-- | Free objects are projective (canonical theorem).
record FreeObjectsAreProjective : Set where
  constructor THEOREM_FreeObjectsAreProjective
  field unit : ⊤

-- | Category has enough projectives.
record HasEnoughProjectives : Set where
  constructor _has_ENOUGH_PROJECTIVES
  field C : M.Identifier

-- | Dual injective theory inferred from projective theory.
record InferDualTheory_Injective : Set where
  constructor INFER_DUAL_THEORY_InjectiveTheory
  field unit : ⊤

------------------------------------------------------------------------
-- Part 7: Injectives and cogenerators
------------------------------------------------------------------------

-- | Injective object declaration.
record InjectiveObject : Set where
  constructor _is_INJECTIVE
  field I : M.Identifier

-- | Extension problem along a monomorphism.
record InjectiveExtensionProblem : Set where
  constructor EXTENSION_PROBLEM_from_mono_WITH_map
  field m f : M.Identifier

-- | Lifting equivalence for injective objects.
record InjectiveLiftingEquivalence : Set where
  constructor THEOREM_InjectiveLiftingEquivalence
  field I : M.Identifier

-- | Cogenerator object declaration.
record CogeneratorObject : Set where
  constructor _is_COGENERATOR
  field C : M.Identifier

-- | Category has enough injectives.
record HasEnoughInjectives : Set where
  constructor _has_ENOUGH_INJECTIVES
  field C : M.Identifier

------------------------------------------------------------------------
-- Bridge postulates: Connect axiom records to typed Proof witnesses
------------------------------------------------------------------------

open C using (Subject; AxiomName; Proof)
open C using (FactorizationS; ProjectiveS; InjectiveS; CategoryPropertyS)
open C using (FactorizationUniquenessName; SubobjectLatticeCompletenessName; CanonicalFactorizationSystemName; ProjectiveLiftingName; InjectiveLiftingName)

postulate
  -- Factorization uniqueness proof
  factorizationUniquenessProof
    : (ax : FactorizationUniquenessAxiom)
    -> (C E M : M.Identifier)
    -> Proof (FactorizationS C E M) FactorizationUniquenessName

  -- Subobject lattice completeness proof
  subobjectLatticeCompleteProof
    : (thm : SubobjectLatticeIsComplete)
    -> (C : M.Identifier)
    -> Proof (CategoryPropertyS C) SubobjectLatticeCompletenessName

  -- Canonical factorization system proof
  canonicalFactorizationProof
    : (thm : CanonicalFactorizationSystem)
    -> (C E M : M.Identifier)
    -> Proof (FactorizationS C E M) CanonicalFactorizationSystemName

  -- Projective lifting proof
  projectiveLiftingProof
    : (obj : ProjectiveObject)
    -> Proof (ProjectiveS (ProjectiveObject.P obj)) ProjectiveLiftingName

  -- Injective lifting proof
  injectiveLiftingProof
    : (obj : InjectiveObject)
    -> Proof (InjectiveS (InjectiveObject.I obj)) InjectiveLiftingName

------------------------------------------------------------------------
-- Notes: Structural encoding of Subobject Theory. CATEGORY prose preserved as
-- comments. Bridge postulates connect axiom records to Core.Proof.
------------------------------------------------------------------------
