-- Tests.FunctorPropertiesChecklist: Test instances for functor limit/colimit properties

module Tests.FunctorPropertiesChecklist where

open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.Equality using (refl)
open import Agda.Builtin.Unit using (⊤)
open import Tests.ObligationAdapters as A
open import Core.CategoricalAdapter


-- Functor properties: preserve/reflect/create limits (6 assertions)

functorPreservesLimitsAdapt : A.FunctorPreservesLimitsAdapter
functorPreservesLimitsAdapt = A.mkFunctorPreservesLimitsAdapter _ _ refl

_ : A.isFilledFunctorPreservesLimits functorPreservesLimitsAdapt ≡ true
_ = refl

-- Categorical assertions for FunctorPreservesLimits
_ : CategoricalAdapter.morphism (A.functorPreservesLimitsCategorical functorPreservesLimitsAdapt) ⊤ ⊤ ≡
    CategoricalAdapter.object (A.functorPreservesLimitsCategorical functorPreservesLimitsAdapt) ⊤
_ = refl

_ : CategoricalAdapter.isomorphism (A.functorPreservesLimitsCategorical functorPreservesLimitsAdapt) ⊤ ⊤ ≡ refl
_ = refl

functorReflectsLimitsAdapt : A.FunctorReflectsLimitsAdapter
functorReflectsLimitsAdapt = A.mkFunctorReflectsLimitsAdapter _ _ refl

_ : A.isFilledFunctorReflectsLimits functorReflectsLimitsAdapt ≡ true
_ = refl

-- Categorical assertions for FunctorReflectsLimits
_ : CategoricalAdapter.morphism (A.functorReflectsLimitsCategorical functorReflectsLimitsAdapt) ⊤ ⊤ ≡
    CategoricalAdapter.object (A.functorReflectsLimitsCategorical functorReflectsLimitsAdapt) ⊤
_ = refl

_ : CategoricalAdapter.isomorphism (A.functorReflectsLimitsCategorical functorReflectsLimitsAdapt) ⊤ ⊤ ≡ refl
_ = refl

functorCreatesLimitsAdapt : A.FunctorCreatesLimitsAdapter
functorCreatesLimitsAdapt = A.mkFunctorCreatesLimitsAdapter _ _ refl

_ : A.isFilledFunctorCreatesLimits functorCreatesLimitsAdapt ≡ true
_ = refl

-- Categorical assertions for FunctorCreatesLimits
_ : CategoricalAdapter.morphism (A.functorCreatesLimitsCategorical functorCreatesLimitsAdapt) ⊤ ⊤ ≡
    CategoricalAdapter.object (A.functorCreatesLimitsCategorical functorCreatesLimitsAdapt) ⊤
_ = refl

_ : CategoricalAdapter.isomorphism (A.functorCreatesLimitsCategorical functorCreatesLimitsAdapt) ⊤ ⊤ ≡ refl
_ = refl

creationImpliesReflectionAdapt : A.CreationImpliesReflectionAdapter
creationImpliesReflectionAdapt = A.mkCreationImpliesReflectionAdapter _ _ refl

_ : A.isFilledCreationImpliesReflection creationImpliesReflectionAdapt ≡ true
_ = refl

-- Categorical assertions for CreationImpliesReflection
_ : CategoricalAdapter.morphism (A.creationImpliesReflectionCategorical creationImpliesReflectionAdapt) ⊤ ⊤ ≡
    CategoricalAdapter.object (A.creationImpliesReflectionCategorical creationImpliesReflectionAdapt) ⊤
_ = refl

_ : CategoricalAdapter.isomorphism (A.creationImpliesReflectionCategorical creationImpliesReflectionAdapt) ⊤ ⊤ ≡ refl
_ = refl

isomorphismsOfCategoriesReflectLimitsAdapt : A.IsomorphismsOfCategoriesReflectLimitsAdapter
isomorphismsOfCategoriesReflectLimitsAdapt = A.mkIsomorphismsOfCategoriesReflectLimitsAdapter _ _ refl

_ : A.isFilledIsomorphismsOfCategoriesReflectLimits isomorphismsOfCategoriesReflectLimitsAdapt ≡ true
_ = refl

-- Categorical assertions for IsomorphismsOfCategoriesReflectLimits
_ : CategoricalAdapter.morphism (A.isomorphismsOfCategoriesReflectLimitsCategorical isomorphismsOfCategoriesReflectLimitsAdapt) ⊤ ⊤ ≡
    CategoricalAdapter.object (A.isomorphismsOfCategoriesReflectLimitsCategorical isomorphismsOfCategoriesReflectLimitsAdapt) ⊤
_ = refl

_ : CategoricalAdapter.isomorphism (A.isomorphismsOfCategoriesReflectLimitsCategorical isomorphismsOfCategoriesReflectLimitsAdapt) ⊤ ⊤ ≡ refl
_ = refl

rightAdjointsPreserveLimits_L2Adapt : A.RightAdjointsPreserveLimits_L2Adapter
rightAdjointsPreserveLimits_L2Adapt = A.mkRightAdjointsPreserveLimits_L2Adapter _ _ refl

_ : A.isFilledRightAdjointsPreserveLimits_L2 rightAdjointsPreserveLimits_L2Adapt ≡ true
_ = refl

-- Categorical assertions for RightAdjointsPreserveLimits_L2
_ : CategoricalAdapter.morphism (A.rightAdjointsPreserveLimits_L2Categorical rightAdjointsPreserveLimits_L2Adapt) ⊤ ⊤ ≡
    CategoricalAdapter.object (A.rightAdjointsPreserveLimits_L2Categorical rightAdjointsPreserveLimits_L2Adapt) ⊤
_ = refl

_ : CategoricalAdapter.isomorphism (A.rightAdjointsPreserveLimits_L2Categorical rightAdjointsPreserveLimits_L2Adapt) ⊤ ⊤ ≡ refl
_ = refl
