module Tests.GrothendieckFibrationsChecklist where

open import Tests.ObligationAdapters as A
open import Agda.Builtin.Bool as B using (Bool)
open import Agda.Builtin.Equality using (refl; _≡_)
open import Agda.Builtin.Unit using (⊤; tt)
open import Chapter2.Level2sub8 as C2S8
open import Chapter1.Level1sub3 as C1S3
import Metamodel as M

-- Grothendieck Fibrations coverage assertions
-- Total: 15 adapters for fibrations, Grothendieck construction, and related structures

emptyFibrationDecl : C2S8.FibrationDeclaration
emptyFibrationDecl = record { projectionFunctor = record { totalCategory = C1S3.CATEGORY (M.mkId "") ; baseCategory = C1S3.CATEGORY (M.mkId "") ; projectionFunctor = M.mkId "" } ; cartesianLiftsExist = ⊤ }

emptyFibrationDeclarationAdapter : A.FibrationDeclarationAdapter
emptyFibrationDeclarationAdapter = A.mkFibrationDeclarationAdapter emptyFibrationDecl (M.mkId "") refl (λ _ → emptyFibrationDecl)

_ : A.isFilledFibrationDeclaration emptyFibrationDeclarationAdapter ≡ B.true
_ = refl

emptyCartesianArrowDecl : C2S8.CartesianArrow
emptyCartesianArrowDecl = record { projectionFunctor = record { totalCategory = C1S3.CATEGORY (M.mkId "") ; baseCategory = C1S3.CATEGORY (M.mkId "") ; projectionFunctor = M.mkId "" } ; arrow = M.mkId "" ; targetObject = M.mkId "" ; baseMorphism = M.mkId "" ; liesOver = record { projectionFunctor = record { totalCategory = C1S3.CATEGORY (M.mkId "") ; baseCategory = C1S3.CATEGORY (M.mkId "") ; projectionFunctor = M.mkId "" } ; morphismInTotal = M.mkId "" ; morphismInBase = M.mkId "" ; projectionEquals = ⊤ } ; universalProperty = ⊤ }

emptyCartesianArrowAdapter : A.CartesianArrowAdapter
emptyCartesianArrowAdapter = A.mkCartesianArrowAdapter emptyCartesianArrowDecl (M.mkId "") refl (λ _ → emptyCartesianArrowDecl)

_ : A.isFilledCartesianArrow emptyCartesianArrowAdapter ≡ B.true
_ = refl

emptyCartesianFunctorDecl : C2S8.CartesianFunctorDeclaration
emptyCartesianFunctorDecl = record { sourceFibration = emptyFibrationDecl ; targetFibration = emptyFibrationDecl ; underlyingFunctor = M.mkId "" ; commutesWithProjections = record { functorF = M.mkId "" ; sourceFibration = emptyFibrationDecl ; targetFibration = emptyFibrationDecl ; diagramCommutes = ⊤ } ; preservesCartesianArrows = record { functorF = M.mkId "" ; sourceFibration = emptyFibrationDecl ; targetFibration = emptyFibrationDecl ; preservesCartesian = ⊤ } }

emptyCartesianFunctorDeclarationAdapter : A.CartesianFunctorDeclarationAdapter
emptyCartesianFunctorDeclarationAdapter = A.mkCartesianFunctorDeclarationAdapter emptyCartesianFunctorDecl (M.mkId "") refl (λ _ → emptyCartesianFunctorDecl)

_ : A.isFilledCartesianFunctorDeclaration emptyCartesianFunctorDeclarationAdapter ≡ B.true
_ = refl

emptyCategoryOfFibrationsDecl : C2S8.CategoryOfFibrations
emptyCategoryOfFibrationsDecl = record { baseCategory = C1S3.CATEGORY (M.mkId "") ; fibrations = ⊤ ; cartesianFunctors = ⊤ ; categoryStructure = C1S3.CATEGORY (M.mkId "") }

emptyCategoryOfFibrationsAdapter : A.CategoryOfFibrationsAdapter
emptyCategoryOfFibrationsAdapter = A.mkCategoryOfFibrationsAdapter emptyCategoryOfFibrationsDecl (C1S3.CATEGORY (M.mkId "")) refl (λ _ → emptyCategoryOfFibrationsDecl)

_ : A.isFilledCategoryOfFibrations emptyCategoryOfFibrationsAdapter ≡ B.true
_ = refl

emptyPseudofunctorFromFibrationDecl : C2S8.PseudofunctorFromFibration
emptyPseudofunctorFromFibrationDecl = record { fibration = emptyFibrationDecl ; underlyingPseudofunctor = ⊤ ; actionOnObjects = ⊤ ; actionOnMorphisms = ⊤ }

emptyPseudofunctorFromFibrationAdapter : A.PseudofunctorFromFibrationAdapter
emptyPseudofunctorFromFibrationAdapter = A.mkPseudofunctorFromFibrationAdapter emptyPseudofunctorFromFibrationDecl (λ _ → emptyPseudofunctorFromFibrationDecl)

_ : A.isFilledPseudofunctorFromFibration emptyPseudofunctorFromFibrationAdapter ≡ B.true
_ = refl

emptyGrothendieckConstructionDecl : C2S8.GrothendieckConstruction
emptyGrothendieckConstructionDecl = record { basePseudofunctor = ⊤ ; totalCategory = C1S3.CATEGORY (M.mkId "") ; projectionFunctor = record { totalCategory = C1S3.CATEGORY (M.mkId "") ; baseCategory = C1S3.CATEGORY (M.mkId "") ; projectionFunctor = M.mkId "" } ; isFibration = emptyFibrationDecl }

emptyGrothendieckConstructionAdapter : A.GrothendieckConstructionAdapter
emptyGrothendieckConstructionAdapter = A.mkGrothendieckConstructionAdapter emptyGrothendieckConstructionDecl (C1S3.CATEGORY (M.mkId "")) refl (λ _ → emptyGrothendieckConstructionDecl)

_ : A.isFilledGrothendieckConstruction emptyGrothendieckConstructionAdapter ≡ B.true
_ = refl

emptyGrothendieckEquivalenceTheoremDecl : C2S8.GrothendieckEquivalenceTheorem
emptyGrothendieckEquivalenceTheoremDecl = record { baseCategory = C1S3.CATEGORY (M.mkId "") ; fibrationsOver = emptyCategoryOfFibrationsDecl ; pseudofunctors = ⊤ ; equivalence = ⊤ }

emptyGrothendieckEquivalenceTheoremAdapter : A.GrothendieckEquivalenceTheoremAdapter
emptyGrothendieckEquivalenceTheoremAdapter = A.mkGrothendieckEquivalenceTheoremAdapter emptyGrothendieckEquivalenceTheoremDecl (C1S3.CATEGORY (M.mkId "")) refl (λ _ → emptyGrothendieckEquivalenceTheoremDecl)

_ : A.isFilledGrothendieckEquivalenceTheorem emptyGrothendieckEquivalenceTheoremAdapter ≡ B.true
_ = refl

emptyFibredAdjunctionDecl : C2S8.FibredAdjunctionDeclaration
emptyFibredAdjunctionDecl = record { leftAdjoint = emptyCartesianFunctorDecl ; rightAdjoint = emptyCartesianFunctorDecl ; sourceFibration = emptyFibrationDecl ; targetFibration = emptyFibrationDecl ; pointwiseAdjunctions = ⊤ }

emptyFibredAdjunctionDeclarationAdapter : A.FibredAdjunctionDeclarationAdapter
emptyFibredAdjunctionDeclarationAdapter = A.mkFibredAdjunctionDeclarationAdapter emptyFibredAdjunctionDecl emptyCartesianFunctorDecl emptyCartesianFunctorDecl refl refl (λ _ → emptyFibredAdjunctionDecl)

_ : A.isFilledFibredAdjunctionDeclaration emptyFibredAdjunctionDeclarationAdapter ≡ B.true
_ = refl

emptyBeckChevalleyConditionDecl : C2S8.BeckChevalleyCondition
emptyBeckChevalleyConditionDecl = record { fibredAdjunction = emptyFibredAdjunctionDecl ; comparisonIsIsomorphism = ⊤ }

emptyBeckChevalleyConditionAdapter : A.BeckChevalleyConditionAdapter
emptyBeckChevalleyConditionAdapter = A.mkBeckChevalleyConditionAdapter emptyBeckChevalleyConditionDecl (λ _ → emptyBeckChevalleyConditionDecl)

_ : A.isFilledBeckChevalleyCondition emptyBeckChevalleyConditionAdapter ≡ B.true
_ = refl

emptyFibrationCompletenessCriterionDecl : C2S8.FibrationCompletenessCriterionTheorem
emptyFibrationCompletenessCriterionDecl = record { fibration = emptyFibrationDecl ; baseIsComplete = ⊤ ; fibresAreComplete = ⊤ ; reindexingPreservesLimits = ⊤ ; totalIsComplete = ⊤ }

emptyFibrationCompletenessCriterionTheoremAdapter : A.FibrationCompletenessCriterionTheoremAdapter
emptyFibrationCompletenessCriterionTheoremAdapter = A.mkFibrationCompletenessCriterionTheoremAdapter emptyFibrationCompletenessCriterionDecl (λ _ → emptyFibrationCompletenessCriterionDecl)

_ : A.isFilledFibrationCompletenessCriterionTheorem emptyFibrationCompletenessCriterionTheoremAdapter ≡ B.true
_ = refl

emptyLocallySmallFibrationDecl : C2S8.LocallySmallFibration
emptyLocallySmallFibrationDecl = record { fibration = emptyFibrationDecl ; allFibresAreSmall = ⊤ }

emptyLocallySmallFibrationAdapter : A.LocallySmallFibrationAdapter
emptyLocallySmallFibrationAdapter = A.mkLocallySmallFibrationAdapter emptyLocallySmallFibrationDecl (λ _ → emptyLocallySmallFibrationDecl)

_ : A.isFilledLocallySmallFibration emptyLocallySmallFibrationAdapter ≡ B.true
_ = refl

emptyRefinedGrothendieckEquivalenceDecl : C2S8.RefinedGrothendieckEquivalenceTheorem
emptyRefinedGrothendieckEquivalenceDecl = record { baseCategory = C1S3.CATEGORY (M.mkId "") ; locallySmallFibrations = ⊤ ; pseudofunctorsToSmallCat = ⊤ ; equivalence = ⊤ }

emptyRefinedGrothendieckEquivalenceTheoremAdapter : A.RefinedGrothendieckEquivalenceTheoremAdapter
emptyRefinedGrothendieckEquivalenceTheoremAdapter = A.mkRefinedGrothendieckEquivalenceTheoremAdapter emptyRefinedGrothendieckEquivalenceDecl (C1S3.CATEGORY (M.mkId "")) refl (λ _ → emptyRefinedGrothendieckEquivalenceDecl)

_ : A.isFilledRefinedGrothendieckEquivalenceTheorem emptyRefinedGrothendieckEquivalenceTheoremAdapter ≡ B.true
_ = refl

emptyCodomainFibrationDecl : C2S8.CodomainFibration
emptyCodomainFibrationDecl = record { baseCategory = C1S3.CATEGORY (M.mkId "") ; arrowCategory = C1S3.CATEGORY (M.mkId "") ; codomainFunctor = M.mkId "" ; isFibration = emptyFibrationDecl }

emptyCodomainFibrationAdapter : A.CodomainFibrationAdapter
emptyCodomainFibrationAdapter = A.mkCodomainFibrationAdapter emptyCodomainFibrationDecl (C1S3.CATEGORY (M.mkId "")) refl (λ _ → emptyCodomainFibrationDecl)

_ : A.isFilledCodomainFibration emptyCodomainFibrationAdapter ≡ B.true
_ = refl

emptyLindenbaumTarskiFibrationDecl : C2S8.LindenbaumTarskiFibration
emptyLindenbaumTarskiFibrationDecl = record { theory = record { language = ⊤ ; axioms = ⊤ } ; baseCategory = C1S3.CATEGORY (M.mkId "") ; totalCategory = C1S3.CATEGORY (M.mkId "") ; projectionFunctor = record { totalCategory = C1S3.CATEGORY (M.mkId "") ; baseCategory = C1S3.CATEGORY (M.mkId "") ; projectionFunctor = M.mkId "" } ; isFibration = emptyFibrationDecl }

emptyLindenbaumTarskiFibrationAdapter : A.LindenbaumTarskiFibrationAdapter
emptyLindenbaumTarskiFibrationAdapter = A.mkLindenbaumTarskiFibrationAdapter emptyLindenbaumTarskiFibrationDecl (λ _ → emptyLindenbaumTarskiFibrationDecl)

_ : A.isFilledLindenbaumTarskiFibration emptyLindenbaumTarskiFibrationAdapter ≡ B.true
_ = refl

emptyFamiliesFibrationDecl : C2S8.FamiliesFibration
emptyFamiliesFibrationDecl = record { baseCategory = C1S3.CATEGORY (M.mkId "") ; familiesCategory = C1S3.CATEGORY (M.mkId "") ; projectionFunctor = record { totalCategory = C1S3.CATEGORY (M.mkId "") ; baseCategory = C1S3.CATEGORY (M.mkId "") ; projectionFunctor = M.mkId "" } ; isFibration = emptyFibrationDecl }

emptyFamiliesFibrationAdapter : A.FamiliesFibrationAdapter
emptyFamiliesFibrationAdapter = A.mkFamiliesFibrationAdapter emptyFamiliesFibrationDecl (C1S3.CATEGORY (M.mkId "")) refl (λ _ → emptyFamiliesFibrationDecl)

_ : A.isFilledFamiliesFibration emptyFamiliesFibrationAdapter ≡ B.true
_ = refl
