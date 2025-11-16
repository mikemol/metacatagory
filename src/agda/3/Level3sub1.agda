-- Level3_1: Locales (Chapter 3, Section 1)
-- This module encodes the structural content of Section 1 from the EBNF grammar.
-- It covers intuitionistic propositional calculus, Heyting algebras, locales, frames,
-- nuclei, open and etale morphisms, compactness, and regularity.

module Chapter3.Level3sub1 where

open import Core

-- ============================================================================
-- Section 1.1: Intuitionistic Propositional Calculus
-- ============================================================================

-- Intuitionistic propositions (syntax)
record IntuitionisticProposition : Set where
  field
    -- Top, bottom, variables, conjunction, disjunction, implication
    propositionData : Set

-- Deduction sequent (Γ ⊢ p)
record DeductionSequent : Set where
  field
    context : Set  -- Set of assumptions
    conclusion : IntuitionisticProposition
    -- Corresponds to a morphism Product(context) → conclusion

-- Inference rules

record AssumptionRule : Set where
  field
    sequent : DeductionSequent
    assumption : IntuitionisticProposition
    -- Projection morphism from context to assumption

record ConjunctionIntroductionRule : Set where
  field
    sequentLeft : DeductionSequent
    sequentRight : DeductionSequent
    -- Universal morphism into product

record ConjunctionEliminationRule : Set where
  field
    sequentConjunction : DeductionSequent
    -- Projection from product

record ImplicationIntroductionRule : Set where
  field
    sequentWithAssumption : DeductionSequent
    -- Currying morphism into exponential

record ImplicationEliminationRule : Set where
  field
    sequentAntecedent : DeductionSequent
    sequentImplication : DeductionSequent
    -- Modus ponens: evaluation morphism

record TruthIntroductionRule : Set where
  field
    -- Unique morphism to terminal object
    trivialProof : Set

record FalsityEliminationRule : Set where
  field
    sequentFalse : DeductionSequent
    targetProposition : IntuitionisticProposition
    -- Unique morphism from initial object

-- ============================================================================
-- Section 1.2: Heyting Algebras
-- ============================================================================

-- Part 1: Definition and Core Adjunction

-- Heyting algebra declaration
record HeytingAlgebraDeclaration : Set where
  field
    underlyingLattice : Set  -- BoundedLatticeDeclaration
    implicationOperation : Set
    -- Adjointness axiom: (c ∧ a ≤ b) ⟺ (c ≤ (a → b))
    adjointnessAxiom : Set

-- Part 2: Structure-Preserving Maps

-- Heyting algebra homomorphism
record HeytingAlgebraHomomorphism : Set where
  field
    sourceAlgebra : HeytingAlgebraDeclaration
    targetAlgebra : HeytingAlgebraDeclaration
    underlyingMorphism : MorphismDeclaration
    -- Preserves lattice operations and implication
    preservesLatticeStructure : Set
    preservesImplication : Set

-- Part 3: Bridge from Logic to Algebra

-- Lindenbaum-Tarski theorem
record LindenbaumTarskiForIPCTheorem : Set where
  field
    -- Set of propositions under provable equivalence forms initial Heyting algebra
    propositionsQuotient : Set
    isHeytingAlgebra : HeytingAlgebraDeclaration
    isInitial : Set

-- ============================================================================
-- Section 1.3: Locales and Frames
-- ============================================================================

-- Frame declaration (complete Heyting algebra)
record FrameDeclaration : Set where
  field
    underlyingHeytingAlgebra : HeytingAlgebraDeclaration
    -- Complete lattice: has all joins and meets
    isCompleteLattice : Set
    -- Frame homomorphisms preserve finite meets and arbitrary joins
    frameStructure : Set

-- Locale declaration
record LocaleDeclaration : Set where
  field
    -- A locale is defined via its frame
    associatedFrame : FrameDeclaration

-- Part 1: The Two Categories

-- Category of frames
record CategoryOfFrames : Set where
  field
    -- Objects: frames
    frames : Set
    -- Morphisms: frame homomorphisms (preserve finite meets, arbitrary joins)
    frameHomomorphisms : Set
    categoryStructure : CategoryDeclaration

-- Category of locales
record CategoryOfLocales : Set where
  field
    -- Objects: locales
    locales : Set
    -- Morphisms: locale morphisms (dual to frame homomorphisms)
    localeMorphisms : Set
    categoryStructure : CategoryDeclaration

-- Part 2: The Core of the Duality - Locale Morphism

-- Locale morphism (represented by frame homomorphism in opposite direction)
record LocaleMorphismDeclaration : Set where
  field
    sourceLocale : LocaleDeclaration
    targetLocale : LocaleDeclaration
    -- A locale morphism L₁ → L₂ is represented by
    -- a frame homomorphism Frame(L₂) → Frame(L₁)
    representingFrameHomomorphism : Set

-- Part 3: Foundational Duality Theorem

-- Locale-Frame duality theorem
record LocaleFrameDualityTheorem : Set where
  field
    -- Loc ≅ Frm^op
    localeCategory : CategoryOfLocales
    frameCategory : CategoryOfFrames
    isOppositeCategory : Set

-- ============================================================================
-- Section 1.4: Completeness and Cocompleteness
-- ============================================================================

-- Theorem: Frm is complete
record FrmIsCompleteTheorem : Set where
  field
    frameCategory : CategoryOfFrames
    hasAllSmallLimits : Set

-- Dual theorem: Loc is cocomplete (derived via duality)
record LocIsCocompl eteTheorem : Set where
  field
    localeCategory : CategoryOfLocales
    hasAllSmallColimits : Set
    -- Derived from FrmIsComplete via duality

-- Locale coproduct via frame product
record LocaleCoproductViaFrameProductTheorem : Set where
  field
    locale1 : LocaleDeclaration
    locale2 : LocaleDeclaration
    -- Frame(L₁ ∐ L₂) ≅ Frame(L₁) × Frame(L₂)
    coproductFormula : Set

-- ============================================================================
-- Section 1.5: Nuclei
-- ============================================================================

-- Part 1: Formal Definition of a Nucleus

-- Nucleus declaration (closure operator on frame)
record NucleusDeclaration : Set where
  field
    frame : FrameDeclaration
    nucleusMap : Set  -- j : F → F
    -- Axiom 1: Inflationary (a ≤ j(a))
    inflationaryAxiom : Set
    -- Axiom 2: Monotone (a ≤ b ⇒ j(a) ≤ j(b))
    monotoneAxiom : Set
    -- Axiom 3: Idempotent (j(j(a)) = j(a))
    idempotentAxiom : Set
    -- Axiom 4: Preserves meets (j(a ∧ b) = j(a) ∧ j(b))
    preservesMeetsAxiom : Set

-- Part 2: Construction of Sublocale's Frame

-- Frame of fixed points
record FrameOfFixedPoints : Set where
  field
    nucleus : NucleusDeclaration
    -- Fix(j) is the frame of j-closed elements
    fixedPointsCarrier : Set
    -- Operations inherited/modified
    meetInherited : Set
    joinClosed : Set  -- j(Join(S))
    isFrame : FrameDeclaration

-- Part 3: Main Correspondence Theorem

-- Sublocale declaration
record SublocaleDeclaration : Set where
  field
    sublocale : LocaleDeclaration
    parentLocale : LocaleDeclaration
    -- Corresponds to monomorphism in Loc (surjection in Frm)
    isMonomorphism : Set

-- Sublocale-nucleus correspondence theorem
record SublocaleNucleusCorrespondenceTheorem : Set where
  field
    locale : LocaleDeclaration
    -- Bijection between sublocales and nuclei
    sublocales : Set
    nuclei : Set
    bijection : Set

-- ============================================================================
-- Section 1.6 & 1.7: Open and Etale Morphisms
-- ============================================================================

-- Part 1: Open Locale Morphisms

-- Direct image functor (left adjoint to frame homomorphism)
record DirectImageFunctorOfFrameHom : Set where
  field
    frameHomomorphism : Set
    -- g_! is left adjoint to g
    leftAdjoint : Set

-- Functor preserves finite meets
record FunctorPropertyPreservesMeets : Set where
  field
    functor : FunctorDeclaration
    -- Preserves top and binary meets
    preservesTop : Set
    preservesBinaryMeets : Set

-- Open locale morphism
record OpenLocaleMorphismDeclaration : Set where
  field
    localeMorphism : LocaleMorphismDeclaration
    -- Underlying frame homomorphism has left adjoint
    hasLeftAdjoint : Set
    -- Left adjoint preserves finite meets
    leftAdjointPreservesMeets : FunctorPropertyPreservesMeets

-- Part 2: Etale Locale Morphisms

-- Beck-Chevalley condition
record BeckChevalleyCondition : Set where
  field
    adjunction : AdjunctionDeclaration
    -- For pullback squares, base change map is isomorphism
    baseChangeIsIsomorphism : Set

-- Etale locale morphism
record EtaleLocaleMorphismDeclaration : Set where
  field
    openMorphism : OpenLocaleMorphismDeclaration
    -- Adjunction satisfies Beck-Chevalley condition
    beckChevalley : BeckChevalleyCondition

-- Part 3: Geometric Equivalence Theorem

-- Theorem: Etale ⟺ Local homeomorphism
record EtaleIsLocalHomeomorphismTheorem : Set where
  field
    continuousMap : Set  -- Between sober spaces
    isLocalHomeomorphism : Set
    correspondingLocaleMorphism : LocaleMorphismDeclaration
    isEtale : EtaleLocaleMorphismDeclaration
    equivalence : Set

-- ============================================================================
-- Section 1.8 & 1.9: Topology and Locale Theory Connection
-- ============================================================================

-- Part 1: Functors Bridging the Two Worlds

-- Ω functor: Top → Loc (open sets functor)
record OmegaFunctor : Set where
  field
    -- Takes topological space to its locale of open sets
    actionOnSpaces : Set
    -- Takes continuous map to locale morphism via inverse image
    actionOnMaps : Set
    underlyingFunctor : FunctorDeclaration

-- pt functor: Loc → Top (points functor)
record PointsFunctor : Set where
  field
    -- Takes locale to space of frame homomorphisms to {0,1}
    actionOnLocales : Set
    -- Takes locale morphism to continuous map
    actionOnMorphisms : Set
    underlyingFunctor : FunctorDeclaration

-- Part 2: Main Adjunction and Unit/Counit

-- Theorem: Ω ⊣ pt
record OmegaIsLeftAdjointToPointsTheorem : Set where
  field
    omegaFunctor : OmegaFunctor
    pointsFunctor : PointsFunctor
    adjunction : AdjunctionDeclaration
    -- Hom_Loc(Ω(X), L) ≅ Hom_Top(X, pt(L))
    homIsomorphism : Set

-- Unit of Ω ⊣ pt adjunction
record UnitOfOmegaPtAdjunction : Set where
  field
    -- η : Id_Top ⇒ pt ∘ Ω
    -- Maps point x to frame homomorphism p_x
    naturalTransformation : Set
    componentMaps : Set

-- Counit of Ω ⊣ pt adjunction
record CounitOfOmegaPtAdjunction : Set where
  field
    -- ε : Ω ∘ pt ⇒ Id_Loc
    -- Maps frame element to open set of points satisfying it
    naturalTransformation : Set
    componentMaps : Set

-- Part 3: Sober Spaces, Spatial Locales, Equivalence

-- Sober space (unit is isomorphism)
record SoberSpaceDeclaration : Set where
  field
    space : Set  -- Topological space
    unitComponent : Set  -- η_X
    unitIsIsomorphism : Set

-- Spatial locale (counit is isomorphism)
record SpatialLocaleDeclaration : Set where
  field
    locale : LocaleDeclaration
    counitComponent : Set  -- ε_L
    counitIsIsomorphism : Set

-- Category of sober spaces
record CategoryOfSoberSpaces : Set where
  field
    soberSpaces : Set
    categoryStructure : CategoryDeclaration

-- Category of spatial locales
record CategoryOfSpatialLocales : Set where
  field
    spatialLocales : Set
    categoryStructure : CategoryDeclaration

-- Sober-Spatial equivalence theorem
record SoberSpatialEquivalenceTheorem : Set where
  field
    soberCategory : CategoryOfSoberSpaces
    spatialCategory : CategoryOfSpatialLocales
    restrictedOmega : FunctorDeclaration
    restrictedPt : FunctorDeclaration
    equivalence : Set

-- ============================================================================
-- Section 1.10 & 1.11: Compactness and Regularity
-- ============================================================================

-- Part 1: Compactness (Point-Free)

-- Frame cover
record FrameCover : Set where
  field
    frame : FrameDeclaration
    coveringSet : Set
    -- Join of covering set equals top element
    joinsToTop : Set

-- Finite subcover
record FiniteSubcover : Set where
  field
    originalCover : FrameCover
    finiteSubset : Set
    isFinite : Set
    stillCovers : Set

-- Compact frame
record CompactFrameDeclaration : Set where
  field
    frame : FrameDeclaration
    -- Every cover has a finite subcover
    everyCoversHasFiniteSubcover : Set

-- Compact locale
record CompactLocaleDeclaration : Set where
  field
    locale : LocaleDeclaration
    -- Frame is compact
    frameIsCompact : CompactFrameDeclaration

-- Theorem: Correspondence of compactness
record CorrespondenceOfCompactnessTheorem : Set where
  field
    soberSpace : SoberSpaceDeclaration
    isTopologicallyCompact : Set
    localeOfOpens : LocaleDeclaration
    isLocalicallyCompact : CompactLocaleDeclaration
    equivalence : Set

-- Part 2: Regularity (Point-Free)

-- Way below relation (b ≺ a)
record WayBelowRelation : Set where
  field
    frame : FrameDeclaration
    elementB : Set
    elementA : Set
    -- ∃c such that b ∧ c = 0 and a ∨ c = 1
    separatingElement : Set

-- Regular frame
record RegularFrameDeclaration : Set where
  field
    frame : FrameDeclaration
    -- Every element is join of elements way below it
    regularityCondition : Set

-- Regular locale
record RegularLocaleDeclaration : Set where
  field
    locale : LocaleDeclaration
    -- Frame is regular
    frameIsRegular : RegularFrameDeclaration

-- Theorem: Correspondence of regularity
record CorrespondenceOfRegularityTheorem : Set where
  field
    soberSpace : SoberSpaceDeclaration
    isTopologicallyRegular : Set
    localeOfOpens : LocaleDeclaration
    isLocalicallyRegular : RegularLocaleDeclaration
    equivalence : Set

-- ============================================================================
-- Instances and Examples
-- ============================================================================

-- Example: Set as a Heyting algebra
record SetAsHeytingAlgebraInstance : Set where
  field
    underlyingSet : Set
    subsetLattice : Set
    implicationViaComplement : Set
    isHeytingAlgebra : HeytingAlgebraDeclaration

-- Example: Frame of open sets of a topological space
record FrameOfOpenSetsInstance : Set where
  field
    topologicalSpace : Set
    openSetsLattice : Set
    isFrame : FrameDeclaration

-- Example: Compact Hausdorff spaces are sober
record CompactHausdorffIsSoberInstance : Set where
  field
    space : Set
    isCompactHausdorff : Set
    isSober : SoberSpaceDeclaration

-- ============================================================================
-- End of Level3_1
-- ============================================================================
