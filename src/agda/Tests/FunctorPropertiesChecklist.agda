-- Tests.FunctorPropertiesChecklist: Test instances for functor limit/colimit properties

module Tests.FunctorPropertiesChecklist where

open import Core.Phase using (Bool; true; false)
open import Agda.Builtin.Equality using (_≡_; refl)
open import Tests.ObligationAdapters as A
open import Metamodel as M
open import Chapter1.Level1sub2 as C1S2


-- Functor properties: preserve/reflect/create limits (6 assertions)

functorPreservesLimitsAdapt : A.FunctorPreservesLimitsAdapter
functorPreservesLimitsAdapt =
  A.mkFunctorPreservesLimitsAdapter (record { F = M.mkId "F" }) (M.mkId "F") refl

_ : A.isFilledFunctorPreservesLimits functorPreservesLimitsAdapt ≡ true
_ = refl

functorReflectsLimitsAdapt : A.FunctorReflectsLimitsAdapter
functorReflectsLimitsAdapt =
  A.mkFunctorReflectsLimitsAdapter (record { F = M.mkId "F" }) (M.mkId "F") refl

_ : A.isFilledFunctorReflectsLimits functorReflectsLimitsAdapt ≡ true
_ = refl

functorCreatesLimitsAdapt : A.FunctorCreatesLimitsAdapter
functorCreatesLimitsAdapt =
  A.mkFunctorCreatesLimitsAdapter (record { F = M.mkId "F" }) (M.mkId "F") refl

_ : A.isFilledFunctorCreatesLimits functorCreatesLimitsAdapt ≡ true
_ = refl

creationImpliesReflectionAdapt : A.CreationImpliesReflectionAdapter
creationImpliesReflectionAdapt =
  A.mkCreationImpliesReflectionAdapter (record { F = M.mkId "F" }) (M.mkId "F") refl

_ : A.isFilledCreationImpliesReflection creationImpliesReflectionAdapt ≡ true
_ = refl

isomorphismsOfCategoriesReflectLimitsAdapt : A.IsomorphismsOfCategoriesReflectLimitsAdapter
isomorphismsOfCategoriesReflectLimitsAdapt =
  A.mkIsomorphismsOfCategoriesReflectLimitsAdapter (record { F = M.mkId "F" }) (M.mkId "F") refl

_ : A.isFilledIsomorphismsOfCategoriesReflectLimits isomorphismsOfCategoriesReflectLimitsAdapt ≡ true
_ = refl

rightAdjointsPreserveLimits_L2Adapt : A.RightAdjointsPreserveLimits_L2Adapter
rightAdjointsPreserveLimits_L2Adapt =
  A.mkRightAdjointsPreserveLimits_L2Adapter (record { F = M.mkId "F" }) (M.mkId "F") refl

_ : A.isFilledRightAdjointsPreserveLimits_L2 rightAdjointsPreserveLimits_L2Adapt ≡ true
_ = refl
