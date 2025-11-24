module Chapter1.Level1sub3 where

open import Agda.Builtin.Unit     using (⊤; tt)
open import Agda.Builtin.String   using (String)
open import Agda.Builtin.List     using (List; []; _∷_)
open import Core.Phase using (Bool; true; false)
open import Agda.Builtin.Equality using (_≡_)

open import Metamodel as M
open import Core      as C

-- Lightweight expression placeholder for formulæ in CATEGORY prose
AdjExpr : Set
AdjExpr = String

------------------------------------------------------------------------
-- Part 1: Two core definitions of adjunction
------------------------------------------------------------------------

-- Hom-set style declaration F ⊣ G : (C, D)
record AdjunctionHomDecl : Set where
  constructor ADJUNCTION_HOM_on
  field F G C D : M.Identifier
-- CATEGORY: F ⊣ G declared via hom-set natural isomorphism.

-- Unit–counit data
record UnitCounitPair : Set where
  constructor UNIT_COϵ
  field η ε : M.Identifier  -- natural transformations (by name)

record TriangleIdentitiesAxiom : Set where
  constructor AXIOM_TriangleIdentities
  field pair : UnitCounitPair

-- Unit–counit style adjunction declaration
record AdjunctionFromUnitCounit : Set where
  constructor ADJUNCTION_from
  field datum : UnitCounitPair

-- Hom-set natural isomorphism witness
record HomSetIsomorphism : Set where
  constructor Hom_≅Hom
  field
    C D : M.Identifier
    F G : M.Identifier

-- Equivalence of the two definitions
record EquivalenceOfAdjunctionDefinitions : Set where
  constructor THEOREM_AdjunctionEquivalence
  field F G C D : M.Identifier

-- Example 1: Free–Forgetful
record CategoryDeclaration : Set where
  constructor CATEGORY
  field name : M.Identifier

record ForgetfulFunctor : Set where
  constructor FORGET_from_to
  field U src dst : M.Identifier

record FreeFunctor : Set where
  constructor FREE_on_from_to
  field F on src dst : M.Identifier

record SpecializedAdjunction_FreeForget : Set where
  constructor FREE_⊣_FORGET
  field F U : M.Identifier

record UnitComponent : Set where
  constructor η_
  field S : M.Identifier

record CounitComponent : Set where
  constructor ε_
  field G : M.Identifier

-- Example 2: Δ ⊣ Π
record ProductCategory : Set where
  constructor PRODUCT_CATEGORY
  field C1 C2 : M.Identifier

record Functor_Delta : Set where
  constructor Δ
  field C : M.Identifier

record Functor_Pi : Set where
  constructor Π
  field C : M.Identifier

record Adjunction_Delta_Pi : Set where
  constructor Δ_⊣_Π
  field C : M.Identifier

------------------------------------------------------------------------
-- Part 2: Adjoint Functor Theorems (left) and duals (right)
------------------------------------------------------------------------

record SolutionSetCondition : Set where
  constructor _satisfies_SolutionSetCondition
  field G : M.Identifier

record WellPowered : Set where
  constructor _is_WELL_POWERED
  field C : M.Identifier

record FunctorProperty_Faithful : Set where
  constructor _is_FAITHFUL
  field F : M.Identifier

record HasCogenerator : Set where
  constructor _has_COGENERATOR
  field D : M.Identifier

-- GAFT / SAFT (left adjoints)
data AdjointFunctorTheoremLeft : Set where
  GAFT : M.Identifier -> AdjointFunctorTheoremLeft  -- parameterize by G id
  SAFT : M.Identifier -> AdjointFunctorTheoremLeft

-- Dual inference to right adjoints
record AdjointFunctorTheoremRight : Set where
  constructor INFER_DUAL_THEOREM_GAFT_SAFT
  field unit : ⊤

------------------------------------------------------------------------
-- Part 3: Properties of adjoint functors
------------------------------------------------------------------------

record RightAdjointsPreserveLimits : Set where
  constructor THEOREM_RightAdjointsPreserveLimits
  field G : M.Identifier

record LeftAdjointsPreserveColimits : Set where
  constructor INFER_DUAL_THEOREM_LeftAdjointsPreserveColimits
  field unit : ⊤

-- Universal arrows via comma categories
record UniversalArrowCategory_X↓G : Set where
  constructor _X_↓_G
  field X G : M.Identifier

record UniversalArrowCategory_F↓Y : Set where
  constructor _F_↓_Y
  field F Y : M.Identifier

record AdjunctionsViaUniversalArrows : Set where
  constructor THEOREM_AdjunctionsViaUniversalArrows
  field F G : M.Identifier

record AdjointsAreUnique : Set where
  constructor THEOREM_AdjointsAreUnique
  field F F′ G : M.Identifier

------------------------------------------------------------------------
-- Part 4: Fully faithful adjoint functors
------------------------------------------------------------------------

record FunctorProperty_Full : Set where
  constructor _is_FULL
  field F : M.Identifier

record FunctorProperty_FullyFaithful : Set where
  constructor _is_FULLY_FAITHFUL
  field F : M.Identifier

record RightAdjointFaithfulnessCriteria : Set where
  constructor THEOREM_RightAdjointFaithfulnessCriteria
  field F G : M.Identifier

record LeftAdjointFaithfulnessCriteria : Set where
  constructor INFER_DUAL_THEOREM_LeftAdjointFaithfulnessCriteria
  field unit : ⊤

------------------------------------------------------------------------
-- Part 5: Reflective subcategories
------------------------------------------------------------------------

record FullSubcategoryDeclaration : Set where
  constructor FULL_SUBCATEGORY_of_on
  field A B : M.Identifier
        objs : List M.Identifier

record InclusionFunctorDeclaration : Set where
  constructor INCLUSION_↪
  field I A B : M.Identifier

record ReflectiveSubcategoryDeclaration : Set where
  constructor _is_REFLECTIVE_IN_
  field A B : M.Identifier

record ReflectorFunctor : Set where
  constructor Reflector
  field A B : M.Identifier

record ReflectionArrow : Set where
  constructor reflection_of
  field b : M.Identifier

record ReflectionCounitIsomorphism : Set where
  constructor THEOREM_ReflectionCounitIsomorphism
  field A B : M.Identifier

------------------------------------------------------------------------
-- Part 6: Epireflective subcategories and duals
------------------------------------------------------------------------

record EpireflectiveSubcategoryDeclaration : Set where
  constructor _is_EPIREFLECTIVE_IN_
  field A B : M.Identifier

-- Closedness properties

data ClosednessKind : Set where
  PRODUCTS   : ClosednessKind
  SUBOBJECTS : ClosednessKind

record CategoryClosednessProperty : Set where
  constructor _is_closed_under_within_
  field A kind B : M.Identifier

record EpireflectiveCriterion : Set where
  constructor THEOREM_EpireflectiveCriterion
  field A B : M.Identifier

record Example_Grp_within_Mon_Epireflective : Set where
  constructor THEOREM_Grp_is_EPIREFLECTIVE_IN_Mon
  field unit : ⊤

-- Dual notions
record MonoreflectiveSubcategoryDeclaration : Set where
  constructor _is_MONOREFLECTIVE_IN_
  field A B : M.Identifier

record MonoreflectiveCriterion : Set where
  constructor INFER_DUAL_THEOREM_MonoreflectiveCriterion
  field unit : ⊤

------------------------------------------------------------------------
-- Part 7: Kan extensions
------------------------------------------------------------------------

record KanExtensionContext : Set where
  constructor CONTEXT_KAN
  field K T : M.Identifier
        C A B : M.Identifier

record LeftKanCandidate : Set where
  constructor L_CANDIDATE
  field M β : M.Identifier

record RightKanCandidate : Set where
  constructor R_CANDIDATE
  field M δ : M.Identifier

record LeftKanExtensionIsInitialObject : Set where
  constructor LeftKanExtension_IS_INITIAL
  field K T : M.Identifier

record RightKanExtensionIsTerminalObject : Set where
  constructor RightKanExtension_IS_TERMINAL
  field K T : M.Identifier

record PointwiseKanFormulaTheorem : Set where
  constructor THEOREM_KanExtensionAsLimit
  field K T : M.Identifier

record CommaObjectUnderFunctor : Set where
  constructor _↓_
  field a K : M.Identifier

record AdjointsAsKanExtensions : Set where
  constructor THEOREM_AdjointsAsKanExtensions
  field F G : M.Identifier

------------------------------------------------------------------------
-- Part 8: Tensor product of set-valued functors
------------------------------------------------------------------------

record PresheafDeclaration : Set where
  constructor PRESHEAF_on
  field F C : M.Identifier

record CopresheafDeclaration : Set where
  constructor COPRESHEAF_on
  field G C : M.Identifier

record TensorAsCoequalizer : Set where
  constructor DEFINE_⊗_as_Coequalizer
  field F G C α β : M.Identifier

record TensorSummand_Objects : Set where
  constructor SumOverObjects
  field F G : M.Identifier

record TensorSummand_Morphisms : Set where
  constructor SumOverMorphisms
  field F G : M.Identifier

record TensorParallelArrowsAxiom : Set where
  constructor AXIOM_TensorParallelArrows
  field alpha beta : M.Identifier
        S_mor S_obj : M.Identifier

record TensorHomAdjunctionTheorem : Set where
  constructor THEOREM_TensorHomAdjunction
  field C G : M.Identifier

record C_BalancedMapDeclaration : Set where
  constructor C_BALANCED_MAP_TO
  field β F G S : M.Identifier

record TensorUniversalPropertyTheorem : Set where
  constructor THEOREM_TensorUP
  field F G S : M.Identifier

------------------------------------------------------------------------
-- Bridge postulates: Level 3 theorems to Core.Proof
------------------------------------------------------------------------

postulate
  -- Triangle identities proof bridge
  triangleIdentitiesProof
    : (adjDecl : AdjunctionHomDecl)  -- or UnitCounitPair with metadata
    -> (ax : TriangleIdentitiesAxiom)
    -> C.Proof (C.AdjunctionS (AdjunctionHomDecl.F adjDecl)
                               (AdjunctionHomDecl.G adjDecl)
                               (AdjunctionHomDecl.C adjDecl)
                               (AdjunctionHomDecl.D adjDecl))
               C.TriangleIdentitiesName

  -- Right adjoint preserves limits
  rightAdjPreservesLimitsProof
    : (adjDecl : AdjunctionHomDecl)
    -> (thm : RightAdjointsPreserveLimits)
    -> C.Proof (C.AdjunctionS (AdjunctionHomDecl.F adjDecl)
                               (RightAdjointsPreserveLimits.G thm)
                               (AdjunctionHomDecl.C adjDecl)
                               (AdjunctionHomDecl.D adjDecl))
               C.RightAdjPreservesLimitsName

  -- Left adjoint preserves colimits (dual)
  leftAdjPreservesColimitsProof
    : (adjDecl : AdjunctionHomDecl)
    -> (thm : LeftAdjointsPreserveColimits)
    -> C.Proof (C.AdjunctionS (AdjunctionHomDecl.F adjDecl)
                               (AdjunctionHomDecl.G adjDecl)
                               (AdjunctionHomDecl.C adjDecl)
                               (AdjunctionHomDecl.D adjDecl))
               C.LeftAdjPreservesColimitsName

------------------------------------------------------------------------
-- Notes: Structural encoding of Adjunctions. CATEGORY narratives preserved
-- as comments near each construct. Bridge functions into the unified proof
-- layer (Core.Proof) can be added incrementally where needed.
------------------------------------------------------------------------
