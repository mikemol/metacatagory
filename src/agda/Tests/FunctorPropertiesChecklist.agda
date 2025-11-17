-- Tests.FunctorPropertiesChecklist: Test instances for functor limit/colimit properties

module Tests.FunctorPropertiesChecklist where

open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.Equality using (refl)
open import Tests.ObligationAdapters as A

-- Functor properties: preserve/reflect/create limits (6 assertions)

functorPreservesLimitsAdapt : A.FunctorPreservesLimitsAdapter
functorPreservesLimitsAdapt = A.mkFunctorPreservesLimitsAdapter _ _ refl

_ : A.isFilledFunctorPreservesLimits functorPreservesLimitsAdapt ≡ true
_ = refl

functorReflectsLimitsAdapt : A.FunctorReflectsLimitsAdapter
functorReflectsLimitsAdapt = A.mkFunctorReflectsLimitsAdapter _ _ refl

_ : A.isFilledFunctorReflectsLimits functorReflectsLimitsAdapt ≡ true
_ = refl

functorCreatesLimitsAdapt : A.FunctorCreatesLimitsAdapter
functorCreatesLimitsAdapt = A.mkFunctorCreatesLimitsAdapter _ _ refl

_ : A.isFilledFunctorCreatesLimits functorCreatesLimitsAdapt ≡ true
_ = refl

creationImpliesReflectionAdapt : A.CreationImpliesReflectionAdapter
creationImpliesReflectionAdapt = A.mkCreationImpliesReflectionAdapter _ _ refl

_ : A.isFilledCreationImpliesReflection creationImpliesReflectionAdapt ≡ true
_ = refl

isomorphismsOfCategoriesReflectLimitsAdapt : A.IsomorphismsOfCategoriesReflectLimitsAdapter
isomorphismsOfCategoriesReflectLimitsAdapt = A.mkIsomorphismsOfCategoriesReflectLimitsAdapter _ _ refl

_ : A.isFilledIsomorphismsOfCategoriesReflectLimits isomorphismsOfCategoriesReflectLimitsAdapt ≡ true
_ = refl

rightAdjointsPreserveLimits_L2Adapt : A.RightAdjointsPreserveLimits_L2Adapter
rightAdjointsPreserveLimits_L2Adapt = A.mkRightAdjointsPreserveLimits_L2Adapter _ _ refl

_ : A.isFilledRightAdjointsPreserveLimits_L2 rightAdjointsPreserveLimits_L2Adapt ≡ true
_ = refl
