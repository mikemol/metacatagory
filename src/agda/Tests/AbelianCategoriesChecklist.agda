module Tests.AbelianCategoriesChecklist where

open import Tests.ObligationAdapters as A
open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.Equality using (refl; _≡_)

-- Abelian Categories coverage assertions
-- Total: 11 adapters for zero objects, kernels, biproducts, additive/abelian categories

emptyHasZeroObjectPropertyAdapter : A.HasZeroObjectPropertyAdapter
emptyHasZeroObjectPropertyAdapter = record { decl = record { category = "" ; zeroObj = "" } ; expectedCategory = "" ; expectedZero = "" ; link1 = refl ; link2 = refl ; status = false }

_ : A.isFilledHasZeroObjectProperty emptyHasZeroObjectPropertyAdapter ≡ true
_ = refl

emptyKernelAsEqualizerDefinitionAdapter : A.KernelAsEqualizerDefinitionAdapter
emptyKernelAsEqualizerDefinitionAdapter = record { decl = record { morphism = "" ; domain = "" ; codomain = "" ; zeroMorphism = record { from = "" ; to = "" ; viaZeroObject = "" ; factorizationLeft = "" ; factorizationRight = "" } ; equalizerObject = "" ; equalizerMono = "" } ; expectedMorphism = "" ; expectedKernel = "" ; link1 = refl ; link2 = refl ; status = false }

_ : A.isFilledKernelAsEqualizerDefinition emptyKernelAsEqualizerDefinitionAdapter ≡ true
_ = refl

emptyBiproductObjectAdapter : A.BiproductObjectAdapter
emptyBiproductObjectAdapter = record { decl = record { left = "" ; right = "" ; object = "" ; projectionLeft = "" ; projectionRight = "" ; injectionLeft = "" ; injectionRight = "" } ; expectedLeft = "" ; expectedRight = "" ; expectedObject = "" ; link1 = refl ; link2 = refl ; link3 = refl ; status = false }

_ : A.isFilledBiproductObject emptyBiproductObjectAdapter ≡ true
_ = refl

emptyAdditiveCategoryDeclarationAdapter : A.AdditiveCategoryDeclarationAdapter
emptyAdditiveCategoryDeclarationAdapter = record { decl = record { category = "" ; hasZeroObject = record { category = "" ; zeroObj = "" } ; enrichment = record { category = "" ; monoidal = "" } ; biproductWitnesses = [] } ; expectedCategory = "" ; link = refl ; status = false }

_ : A.isFilledAdditiveCategoryDeclaration emptyAdditiveCategoryDeclarationAdapter ≡ true
_ = refl

emptyAbelianCategoryDeclarationAdapter : A.AbelianCategoryDeclarationAdapter
emptyAbelianCategoryDeclarationAdapter = record { decl = record { category = "" ; additive = record { category = "" ; hasZeroObject = record { category = "" ; zeroObj = "" } ; enrichment = record { category = "" ; monoidal = "" } ; biproductWitnesses = [] } ; hasAllKernels = false ; hasAllCokernels = false ; monosNormalWitness = false ; episNormalWitness = false } ; expectedCategory = "" ; link = refl ; status = false }

_ : A.isFilledAbelianCategoryDeclaration emptyAbelianCategoryDeclarationAdapter ≡ true
_ = refl

emptyFirstIsomorphismForAbelianCategoriesTheoremAdapter : A.FirstIsomorphismForAbelianCategoriesTheoremAdapter
emptyFirstIsomorphismForAbelianCategoriesTheoremAdapter = record { decl = record { category = "" ; morphism = "" ; isomorphismWitness = "" } ; expectedCategory = "" ; expectedMorphism = "" ; link1 = refl ; link2 = refl ; status = false }

_ : A.isFilledFirstIsomorphismForAbelianCategoriesTheorem emptyFirstIsomorphismForAbelianCategoriesTheoremAdapter ≡ true
_ = refl

emptyNormalMonomorphismPropertyAdapter : A.NormalMonomorphismPropertyAdapter
emptyNormalMonomorphismPropertyAdapter = record { decl = record { mono = "" ; kernelCokernelWitness = "" } ; expectedMono = "" ; link = refl ; status = false }

_ : A.isFilledNormalMonomorphismProperty emptyNormalMonomorphismPropertyAdapter ≡ true
_ = refl

emptyAbelianCategoryExampleAbAdapter : A.AbelianCategoryExampleAbAdapter
emptyAbelianCategoryExampleAbAdapter = record { decl = record { unit = _ } ; status = false }

_ : A.isFilledAbelianCategoryExampleAb emptyAbelianCategoryExampleAbAdapter ≡ true
_ = refl

emptyAbelianCategoryExampleRModAdapter : A.AbelianCategoryExampleRModAdapter
emptyAbelianCategoryExampleRModAdapter = record { decl = record { ring = "" ; unit = _ } ; expectedRing = "" ; link = refl ; status = false }

_ : A.isFilledAbelianCategoryExampleRMod emptyAbelianCategoryExampleRModAdapter ≡ true
_ = refl

emptyFunctorAdditivePropertyAdapter : A.FunctorAdditivePropertyAdapter
emptyFunctorAdditivePropertyAdapter = record { decl = record { functor = "" ; source = "" ; target = "" ; homGroupPreservationWitness = "" } ; expectedFunctor = "" ; link = refl ; status = false }

_ : A.isFilledFunctorAdditiveProperty emptyFunctorAdditivePropertyAdapter ≡ true
_ = refl

emptyAdditivityViaBiproductCoincidenceTheoremAdapter : A.AdditivityViaBiproductCoincidenceTheoremAdapter
emptyAdditivityViaBiproductCoincidenceTheoremAdapter = record { decl = record { category = "" ; hasZeroObject = false ; hasFiniteProducts = false ; hasFiniteCoproducts = false ; comparisonMapIsIsoWitness = "" } ; expectedCategory = "" ; link = refl ; status = false }

_ : A.isFilledAdditivityViaBiproductCoincidenceTheorem emptyAdditivityViaBiproductCoincidenceTheoremAdapter ≡ true
_ = refl
