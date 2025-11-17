-- Tests.RegularCategoriesChecklist: Test instances for regular category theory

module Tests.RegularCategoriesChecklist where

open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.Equality using (refl)
open import Tests.ObligationAdapters as A

-- Regular category theory (4 assertions)

regularCategoryDeclarationAdapt : A.RegularCategoryDeclarationAdapter
regularCategoryDeclarationAdapt = A.mkRegularCategoryDeclarationAdapter _ _ _ refl refl

_ : A.isFilledRegularCategoryDeclaration regularCategoryDeclarationAdapt ≡ true
_ = refl

kernelPairDeclarationAdapt : A.KernelPairDeclarationAdapter
kernelPairDeclarationAdapt = A.mkKernelPairDeclarationAdapter _ _ refl

_ : A.isFilledKernelPairDeclaration kernelPairDeclarationAdapt ≡ true
_ = refl

internalEquivalenceRelationDeclarationAdapt : A.InternalEquivalenceRelationDeclarationAdapter
internalEquivalenceRelationDeclarationAdapt = A.mkInternalEquivalenceRelationDeclarationAdapter _ _ refl

_ : A.isFilledInternalEquivalenceRelationDeclaration internalEquivalenceRelationDeclarationAdapt ≡ true
_ = refl

exactCategoryDeclarationAdapt : A.ExactCategoryDeclarationAdapter
exactCategoryDeclarationAdapt = A.mkExactCategoryDeclarationAdapter _ _ refl

_ : A.isFilledExactCategoryDeclaration exactCategoryDeclarationAdapt ≡ true
_ = refl
