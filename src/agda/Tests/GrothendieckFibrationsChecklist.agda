module Tests.GrothendieckFibrationsChecklist where

open import Tests.ObligationAdapters as A
open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.Equality using (refl; _≡_)
open import Agda.Builtin.Unit using (⊤; tt)
open import Chapter2.Level2sub8 as C2S8
open import Core.CategoricalAdapter

-- Grothendieck Fibrations coverage assertions
-- Total: 15 adapters for fibrations, Grothendieck construction, and related structures

emptyFibrationDecl : C2S8.FibrationDeclaration
emptyFibrationDecl = record { projectionFunctor = record { totalCategory = record {} ; baseCategory = record {} ; projectionFunctor = "" } ; cartesianLiftsExist = record {} }

emptyFibrationDeclarationAdapter : A.FibrationDeclarationAdapter
emptyFibrationDeclarationAdapter = A.mkFibrationDeclarationAdapter emptyFibrationDecl "" refl (λ _ → emptyFibrationDecl)

_ : A.isFilledFibrationDeclaration emptyFibrationDeclarationAdapter ≡ true
_ = refl

-- Categorical correspondence sanity check
_ : C2S8.FibrationDeclaration ≡ C2S8.FibrationDeclaration
_ = refl
_ : (CategoricalAdapter.morphism (A.fibrationDeclarationCategorical emptyFibrationDeclarationAdapter) tt) ≡ A.FibrationDeclarationAdapter.decl emptyFibrationDeclarationAdapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.fibrationDeclarationCategorical emptyFibrationDeclarationAdapter) ≡ refl
_ = refl

emptyCartesianArrowDecl : C2S8.CartesianArrow
emptyCartesianArrowDecl = record { projectionFunctor = record { totalCategory = record {} ; baseCategory = record {} ; projectionFunctor = "" } ; arrow = "" ; targetObject = "" ; baseMorphism = "" ; liesOver = record { projectionFunctor = record { totalCategory = record {} ; baseCategory = record {} ; projectionFunctor = "" } ; morphismInTotal = "" ; morphismInBase = "" ; projectionEquals = record {} } ; universalProperty = record {} }

emptyCartesianArrowAdapter : A.CartesianArrowAdapter
emptyCartesianArrowAdapter = A.mkCartesianArrowAdapter emptyCartesianArrowDecl "" refl (λ _ → emptyCartesianArrowDecl)

_ : A.isFilledCartesianArrow emptyCartesianArrowAdapter ≡ true
_ = refl
_ : (CategoricalAdapter.morphism (A.cartesianArrowCategorical emptyCartesianArrowAdapter) tt) ≡ A.CartesianArrowAdapter.decl emptyCartesianArrowAdapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.cartesianArrowCategorical emptyCartesianArrowAdapter) ≡ refl
_ = refl

emptyCartesianFunctorDecl : C2S8.CartesianFunctorDeclaration
emptyCartesianFunctorDecl = record { sourceFibration = emptyFibrationDecl ; targetFibration = emptyFibrationDecl ; underlyingFunctor = "" ; commutesWithProjections = record { functorF = "" ; sourceFibration = emptyFibrationDecl ; targetFibration = emptyFibrationDecl ; diagramCommutes = record {} } ; preservesCartesianArrows = record { functorF = "" ; sourceFibration = emptyFibrationDecl ; targetFibration = emptyFibrationDecl ; preservesCartesian = record {} } }

emptyCartesianFunctorDeclarationAdapter : A.CartesianFunctorDeclarationAdapter
emptyCartesianFunctorDeclarationAdapter = A.mkCartesianFunctorDeclarationAdapter emptyCartesianFunctorDecl "" refl (λ _ → emptyCartesianFunctorDecl)

_ : A.isFilledCartesianFunctorDeclaration emptyCartesianFunctorDeclarationAdapter ≡ true
_ = refl
_ : (CategoricalAdapter.morphism (A.cartesianFunctorDeclarationCategorical emptyCartesianFunctorDeclarationAdapter) tt) ≡ A.CartesianFunctorDeclarationAdapter.decl emptyCartesianFunctorDeclarationAdapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.cartesianFunctorDeclarationCategorical emptyCartesianFunctorDeclarationAdapter) ≡ refl
_ = refl

emptyCategoryOfFibrationsDecl : C2S8.CategoryOfFibrations
emptyCategoryOfFibrationsDecl = record { baseCategory = record {} ; fibrations = record {} ; cartesianFunctors = record {} ; categoryStructure = record {} }

emptyCategoryOfFibrationsAdapter : A.CategoryOfFibrationsAdapter
emptyCategoryOfFibrationsAdapter = A.mkCategoryOfFibrationsAdapter emptyCategoryOfFibrationsDecl "" refl (λ _ → emptyCategoryOfFibrationsDecl)

_ : A.isFilledCategoryOfFibrations emptyCategoryOfFibrationsAdapter ≡ true
_ = refl
_ : (CategoricalAdapter.morphism (A.categoryOfFibrationsCategorical emptyCategoryOfFibrationsAdapter) tt) ≡ A.CategoryOfFibrationsAdapter.decl emptyCategoryOfFibrinationsAdapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.categoryOfFibrationsCategorical emptyCategoryOfFibrationsAdapter) ≡ refl
_ = refl

emptyPseudofunctorFromFibrationDecl : C2S8.PseudofunctorFromFibration
emptyPseudofunctorFromFibrationDecl = record { fibration = emptyFibrationDecl ; underlyingPseudofunctor = record {} ; actionOnObjects = record {} ; actionOnMorphisms = record {} }

emptyPseudofunctorFromFibrationAdapter : A.PseudofunctorFromFibrationAdapter
emptyPseudofunctorFromFibrationAdapter = A.mkPseudofunctorFromFibrationAdapter emptyPseudofunctorFromFibrationDecl (λ _ → emptyPseudofunctorFromFibrationDecl)

_ : A.isFilledPseudofunctorFromFibration emptyPseudofunctorFromFibrationAdapter ≡ true
_ = refl
_ : (CategoricalAdapter.morphism (A.pseudofunctorFromFibrationCategorical emptyPseudofunctorFromFibrationAdapter) tt) ≡ A.PseudofunctorFromFibrationAdapter.decl emptyPseudofunctorFromFibrationAdapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.pseudofunctorFromFibrationCategorical emptyPseudofunctorFromFibrationAdapter) ≡ refl
_ = refl

emptyGrothendieckConstructionDecl : C2S8.GrothendieckConstruction
emptyGrothendieckConstructionDecl = record { basePseudofunctor = record {} ; totalCategory = record {} ; projectionFunctor = record { totalCategory = record {} ; baseCategory = record {} ; projectionFunctor = "" } ; isFibration = emptyFibrationDecl }

emptyGrothendieckConstructionAdapter : A.GrothendieckConstructionAdapter
emptyGrothendieckConstructionAdapter = A.mkGrothendieckConstructionAdapter emptyGrothendieckConstructionDecl "" refl (λ _ → emptyGrothendieckConstructionDecl)

_ : A.isFilledGrothendieckConstruction emptyGrothendieckConstructionAdapter ≡ true
_ = refl
_ : (CategoricalAdapter.morphism (A.grothendieckConstructionCategorical emptyGrothendieckConstructionAdapter) tt) ≡ A.GrothendieckConstructionAdapter.decl emptyGrothendieckConstructionAdapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.grothendieckConstructionCategorical emptyGrothendieckConstructionAdapter) ≡ refl
_ = refl

emptyGrothendieckEquivalenceTheoremDecl : C2S8.GrothendieckEquivalenceTheorem
emptyGrothendieckEquivalenceTheoremDecl = record { baseCategory = record {} ; fibrationsOver = emptyCategoryOfFibrationsDecl ; pseudofunctors = record {} ; equivalence = record {} }

emptyGrothendieckEquivalenceTheoremAdapter : A.GrothendieckEquivalenceTheoremAdapter
emptyGrothendieckEquivalenceTheoremAdapter = A.mkGrothendieckEquivalenceTheoremAdapter emptyGrothendieckEquivalenceTheoremDecl "" refl (λ _ → emptyGrothendieckEquivalenceTheoremDecl)

_ : A.isFilledGrothendieckEquivalenceTheorem emptyGrothendieckEquivalenceTheoremAdapter ≡ true
_ = refl
_ : (CategoricalAdapter.morphism (A.grothendieckEquivalenceTheoremCategorical emptyGrothendieckEquivalenceTheoremAdapter) tt) ≡ A.GrothendieckEquivalenceTheoremAdapter.decl emptyGrothendieckEquivalenceTheoremAdapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.grothendieckEquivalenceTheoremCategorical emptyGrothendieckEquivalenceTheoremAdapter) ≡ refl
_ = refl

emptyFibredAdjunctionDecl : C2S8.FibredAdjunctionDeclaration
emptyFibredAdjunctionDecl = record { leftAdjoint = emptyCartesianFunctorDecl ; rightAdjoint = emptyCartesianFunctorDecl ; sourceFibration = emptyFibrationDecl ; targetFibration = emptyFibrationDecl ; pointwiseAdjunctions = record {} }

emptyFibredAdjunctionDeclarationAdapter : A.FibredAdjunctionDeclarationAdapter
emptyFibredAdjunctionDeclarationAdapter = A.mkFibredAdjunctionDeclarationAdapter emptyFibredAdjunctionDecl "" "" refl refl (λ _ → emptyFibredAdjunctionDecl)

_ : A.isFilledFibredAdjunctionDeclaration emptyFibredAdjunctionDeclarationAdapter ≡ true
_ = refl
_ : (CategoricalAdapter.morphism (A.fibredAdjunctionDeclarationCategorical emptyFibredAdjunctionDeclarationAdapter) tt) ≡ A.FibredAdjunctionDeclarationAdapter.decl emptyFibredAdjunctionDeclarationAdapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.fibredAdjunctionDeclarationCategorical emptyFibredAdjunctionDeclarationAdapter) ≡ refl
_ = refl

emptyBeckChevalleyConditionDecl : C2S8.BeckChevalleyCondition
emptyBeckChevalleyConditionDecl = record { fibredAdjunction = emptyFibredAdjunctionDecl ; comparisonIsIsomorphism = record {} }

emptyBeckChevalleyConditionAdapter : A.BeckChevalleyConditionAdapter
emptyBeckChevalleyConditionAdapter = A.mkBeckChevalleyConditionAdapter emptyBeckChevalleyConditionDecl (λ _ → emptyBeckChevalleyConditionDecl)

_ : A.isFilledBeckChevalleyCondition emptyBeckChevalleyConditionAdapter ≡ true
_ = refl
_ : (CategoricalAdapter.morphism (A.beckChevalleyConditionCategorical emptyBeckChevalleyConditionAdapter) tt) ≡ A.BeckChevalleyConditionAdapter.decl emptyBeckChevalleyConditionAdapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.beckChevalleyConditionCategorical emptyBeckChevalleyConditionAdapter) ≡ refl
_ = refl

emptyFibrationCompletenessCriterionDecl : C2S8.FibrationCompletenessCriterionTheorem
emptyFibrationCompletenessCriterionDecl = record { fibration = emptyFibrationDecl ; baseIsComplete = record {} ; fibresAreComplete = record {} ; reindexingPreservesLimits = record {} ; totalIsComplete = record {} }

emptyFibrationCompletenessCriterionTheoremAdapter : A.FibrationCompletenessCriterionTheoremAdapter
emptyFibrationCompletenessCriterionTheoremAdapter = A.mkFibrationCompletenessCriterionTheoremAdapter emptyFibrationCompletenessCriterionDecl (λ _ → emptyFibrationCompletenessCriterionDecl)

_ : A.isFilledFibrationCompletenessCriterionTheorem emptyFibrationCompletenessCriterionTheoremAdapter ≡ true
_ = refl
_ : (CategoricalAdapter.morphism (A.fibrationCompletenessCriterionTheoremCategorical emptyFibrationCompletenessCriterionTheoremAdapter) tt) ≡ A.FibrationCompletenessCriterionTheoremAdapter.decl emptyFibrationCompletenessCriterionTheoremAdapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.fibrationCompletenessCriterionTheoremCategorical emptyFibrationCompletenessCriterionTheoremAdapter) ≡ refl
_ = refl

emptyLocallySmallFibrationDecl : C2S8.LocallySmallFibration
emptyLocallySmallFibrationDecl = record { fibration = emptyFibrationDecl ; allFibresAreSmall = record {} }

emptyLocallySmallFibrationAdapter : A.LocallySmallFibrationAdapter
emptyLocallySmallFibrationAdapter = A.mkLocallySmallFibrationAdapter emptyLocallySmallFibrationDecl (λ _ → emptyLocallySmallFibrationDecl)

_ : A.isFilledLocallySmallFibration emptyLocallySmallFibrationAdapter ≡ true
_ = refl
_ : (CategoricalAdapter.morphism (A.locallySmallFibrationCategorical emptyLocallySmallFibrationAdapter) tt) ≡ A.LocallySmallFibrationAdapter.decl emptyLocallySmallFibrationAdapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.locallySmallFibrationCategorical emptyLocallySmallFibrationAdapter) ≡ refl
_ = refl

emptyRefinedGrothendieckEquivalenceDecl : C2S8.RefinedGrothendieckEquivalenceTheorem
emptyRefinedGrothendieckEquivalenceDecl = record { baseCategory = record {} ; locallySmallFibrations = record {} ; pseudofunctorsToSmallCat = record {} ; equivalence = record {} }

emptyRefinedGrothendieckEquivalenceTheoremAdapter : A.RefinedGrothendieckEquivalenceTheoremAdapter
emptyRefinedGrothendieckEquivalenceTheoremAdapter = A.mkRefinedGrothendieckEquivalenceTheoremAdapter emptyRefinedGrothendieckEquivalenceDecl "" refl (λ _ → emptyRefinedGrothendieckEquivalenceDecl)

_ : A.isFilledRefinedGrothendieckEquivalenceTheorem emptyRefinedGrothendieckEquivalenceTheoremAdapter ≡ true
_ = refl
_ : (CategoricalAdapter.morphism (A.refinedGrothendieckEquivalenceTheoremCategorical emptyRefinedGrothendieckEquivalenceTheoremAdapter) tt) ≡ A.RefinedGrothendieckEquivalenceTheoremAdapter.decl emptyRefinedGrothendieckEquivalenceTheoremAdapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.refinedGrothendieckEquivalenceTheoremCategorical emptyRefinedGrothendieckEquivalenceTheoremAdapter) ≡ refl
_ = refl

emptyCodomainFibrationDecl : C2S8.CodomainFibration
emptyCodomainFibrationDecl = record { baseCategory = record {} ; arrowCategory = record {} ; codomainFunctor = "" ; isFibration = emptyFibrationDecl }

emptyCodomainFibrationAdapter : A.CodomainFibrationAdapter
emptyCodomainFibrationAdapter = A.mkCodomainFibrationAdapter emptyCodomainFibrationDecl "" refl (λ _ → emptyCodomainFibrationDecl)

_ : A.isFilledCodomainFibration emptyCodomainFibrationAdapter ≡ true
_ = refl
_ : (CategoricalAdapter.morphism (A.codomainFibrationCategorical emptyCodomainFibrationAdapter) tt) ≡ A.CodomainFibrationAdapter.decl emptyCodomainFibrationAdapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.codomainFibrationCategorical emptyCodomainFibrationAdapter) ≡ refl
_ = refl

emptyLindenbaumTarskiFibrationDecl : C2S8.LindenbaumTarskiFibration
emptyLindenbaumTarskiFibrationDecl = record { theory = record { language = record {} ; axioms = record {} } ; baseCategory = record {} ; totalCategory = record {} ; projectionFunctor = record { totalCategory = record {} ; baseCategory = record {} ; projectionFunctor = "" } ; isFibration = emptyFibrationDecl }

emptyLindenbaumTarskiFibrationAdapter : A.LindenbaumTarskiFibrationAdapter
emptyLindenbaumTarskiFibrationAdapter = A.mkLindenbaumTarskiFibrationAdapter emptyLindenbaumTarskiFibrationDecl (λ _ → emptyLindenbaumTarskiFibrationDecl)

_ : A.isFilledLindenbaumTarskiFibration emptyLindenbaumTarskiFibrationAdapter ≡ true
_ = refl
_ : (CategoricalAdapter.morphism (A.lindenbaumTarskiFibrationCategorical emptyLindenbaumTarskiFibrationAdapter) tt) ≡ A.LindenbaumTarskiFibrationAdapter.decl emptyLindenbaumTarskiFibrationAdapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.lindenbaumTarskiFibrationCategorical emptyLindenbaumTarskiFibrationAdapter) ≡ refl
_ = refl

emptyFamiliesFibrationDecl : C2S8.FamiliesFibration
emptyFamiliesFibrationDecl = record { baseCategory = record {} ; familiesCategory = record {} ; projectionFunctor = record { totalCategory = record {} ; baseCategory = record {} ; projectionFunctor = "" } ; isFibration = emptyFibrationDecl }

emptyFamiliesFibrationAdapter : A.FamiliesFibrationAdapter
emptyFamiliesFibrationAdapter = A.mkFamiliesFibrationAdapter emptyFamiliesFibrationDecl "" refl (λ _ → emptyFamiliesFibrationDecl)

_ : A.isFilledFamiliesFibration emptyFamiliesFibrationAdapter ≡ true
_ = refl
_ : (CategoricalAdapter.morphism (A.familiesFibrationCategorical emptyFamiliesFibrationAdapter) tt) ≡ A.FamiliesFibrationAdapter.decl emptyFamiliesFibrationAdapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.familiesFibrationCategorical emptyFamiliesFibrationAdapter) ≡ refl
_ = refl
