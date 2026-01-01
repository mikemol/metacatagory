{-# OPTIONS --without-K #-}

-- | Checklist of obligations for abelian categories.
module Tests.AbelianCategoriesChecklist where

open import Tests.ObligationAdapters as A
open import Core.Phase using (Bool; true; false)
open import Agda.Builtin.Equality using (refl; _≡_)
open import Agda.Builtin.List using (List; []; _∷_)
open import Metamodel using (mkId)

-- Abelian Categories coverage assertions
-- Total: 11 adapters for zero objects, kernels, biproducts, additive/abelian categories

-- Placeholder normalized: status remains false, assertion matches false.
-- TODO: Use mkHasZeroObjectPropertyAdapter with concrete zero object (e.g. 0 in Ab).
emptyHasZeroObjectPropertyAdapter : A.HasZeroObjectPropertyAdapter
emptyHasZeroObjectPropertyAdapter = record { decl = record { category = mkId "" ; zeroObj = mkId "" } ; expectedCategory = mkId "" ; expectedZero = mkId "" ; link1 = refl ; link2 = refl ; status = false }

_ : A.isFilledHasZeroObjectProperty emptyHasZeroObjectPropertyAdapter ≡ false
_ = refl

-- TODO: Provide actual kernel-as-equalizer instance via constructor.
emptyKernelAsEqualizerDefinitionAdapter : A.KernelAsEqualizerDefinitionAdapter
emptyKernelAsEqualizerDefinitionAdapter = record { decl = record { morphism = mkId "" ; domain = mkId "" ; codomain = mkId "" ; zeroMorphism = record { from = mkId "" ; to = mkId "" ; viaZeroObject = mkId "" ; factorizationLeft = mkId "" ; factorizationRight = mkId "" } ; equalizerObject = mkId "" ; equalizerMono = mkId "" } ; expectedMorphism = mkId "" ; expectedKernel = mkId "" ; link1 = refl ; link2 = refl ; status = false }

_ : A.isFilledKernelAsEqualizerDefinition emptyKernelAsEqualizerDefinitionAdapter ≡ false
_ = refl

-- TODO: Replace with actual biproduct (e.g. A⊕B in Ab) via mkBiproductObjectAdapter.
emptyBiproductObjectAdapter : A.BiproductObjectAdapter
emptyBiproductObjectAdapter = record { decl = record { left = mkId "" ; right = mkId "" ; object = mkId "" ; projectionLeft = mkId "" ; projectionRight = mkId "" ; injectionLeft = mkId "" ; injectionRight = mkId "" } ; expectedLeft = mkId "" ; expectedRight = mkId "" ; expectedObject = mkId "" ; link1 = refl ; link2 = refl ; link3 = refl ; status = false }

_ : A.isFilledBiproductObject emptyBiproductObjectAdapter ≡ false
_ = refl

-- TODO: Construct additive category declaration with real witnesses.
emptyAdditiveCategoryDeclarationAdapter : A.AdditiveCategoryDeclarationAdapter
emptyAdditiveCategoryDeclarationAdapter = record { decl = record { category = mkId "" ; hasZeroObject = record { category = mkId "" ; zeroObj = mkId "" } ; enrichment = record { category = mkId "" ; monoidal = mkId "" } ; biproductWitnesses = [] } ; expectedCategory = mkId "" ; link = refl ; status = false }

_ : A.isFilledAdditiveCategoryDeclaration emptyAdditiveCategoryDeclarationAdapter ≡ false
_ = refl

-- TODO: Populate abelian category declaration (e.g., Ab) and switch to constructor.
emptyAbelianCategoryDeclarationAdapter : A.AbelianCategoryDeclarationAdapter
emptyAbelianCategoryDeclarationAdapter = record { decl = record { category = mkId "" ; additive = record { category = mkId "" ; hasZeroObject = record { category = mkId "" ; zeroObj = mkId "" } ; enrichment = record { category = mkId "" ; monoidal = mkId "" } ; biproductWitnesses = [] } ; hasAllKernels = false ; hasAllCokernels = false ; monosNormalWitness = false ; episNormalWitness = false } ; expectedCategory = mkId "" ; link = refl ; status = false }

_ : A.isFilledAbelianCategoryDeclaration emptyAbelianCategoryDeclarationAdapter ≡ false
_ = refl

-- TODO: Provide actual morphism and iso witness.
emptyFirstIsomorphismForAbelianCategoriesTheoremAdapter : A.FirstIsomorphismForAbelianCategoriesTheoremAdapter
emptyFirstIsomorphismForAbelianCategoriesTheoremAdapter = record { decl = record { category = mkId "" ; morphism = mkId "" ; isomorphismWitness = mkId "" } ; expectedCategory = mkId "" ; expectedMorphism = mkId "" ; link1 = refl ; link2 = refl ; status = false }

_ : A.isFilledFirstIsomorphismForAbelianCategoriesTheorem emptyFirstIsomorphismForAbelianCategoriesTheoremAdapter ≡ false
_ = refl

-- TODO: Provide normal mono example via constructor.
emptyNormalMonomorphismPropertyAdapter : A.NormalMonomorphismPropertyAdapter
emptyNormalMonomorphismPropertyAdapter = record { decl = record { mono = mkId "" ; kernelCokernelWitness = mkId "" } ; expectedMono = mkId "" ; link = refl ; status = false }

_ : A.isFilledNormalMonomorphismProperty emptyNormalMonomorphismPropertyAdapter ≡ false
_ = refl

-- TODO: Provide example for Ab category.
emptyAbelianCategoryExampleAbAdapter : A.AbelianCategoryExampleAbAdapter
emptyAbelianCategoryExampleAbAdapter = record { decl = record { unit = _ } ; status = false }

_ : A.isFilledAbelianCategoryExampleAb emptyAbelianCategoryExampleAbAdapter ≡ false
_ = refl

-- TODO: Provide R-Mod category example with explicit ring.
emptyAbelianCategoryExampleRModAdapter : A.AbelianCategoryExampleRModAdapter
emptyAbelianCategoryExampleRModAdapter = record { decl = record { ring = mkId "" ; unit = _ } ; expectedRing = mkId "" ; link = refl ; status = false }

_ : A.isFilledAbelianCategoryExampleRMod emptyAbelianCategoryExampleRModAdapter ≡ false
_ = refl

-- TODO: Populate additive functor (e.g., Hom functor) via constructor adapter.
emptyFunctorAdditivePropertyAdapter : A.FunctorAdditivePropertyAdapter
emptyFunctorAdditivePropertyAdapter = record { decl = record { functor = mkId "" ; source = mkId "" ; target = mkId "" ; homGroupPreservationWitness = mkId "" } ; expectedFunctor = mkId "" ; link = refl ; status = false }

_ : A.isFilledFunctorAdditiveProperty emptyFunctorAdditivePropertyAdapter ≡ false
_ = refl

-- TODO: Provide biproduct coincidence theorem witness.
emptyAdditivityViaBiproductCoincidenceTheoremAdapter : A.AdditivityViaBiproductCoincidenceTheoremAdapter
emptyAdditivityViaBiproductCoincidenceTheoremAdapter = record { decl = record { category = mkId "" ; hasZeroObject = false ; hasFiniteProducts = false ; hasFiniteCoproducts = false ; comparisonMapIsIsoWitness = mkId "" } ; expectedCategory = mkId "" ; link = refl ; status = false }

_ : A.isFilledAdditivityViaBiproductCoincidenceTheorem emptyAdditivityViaBiproductCoincidenceTheoremAdapter ≡ false
_ = refl
