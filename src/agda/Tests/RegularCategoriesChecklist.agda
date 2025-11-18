-- Tests.RegularCategoriesChecklist: Test instances for regular category theory

module Tests.RegularCategoriesChecklist where

open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.Equality using (refl)
open import Agda.Builtin.Unit using (⊤)
open import Tests.ObligationAdapters as A
open import Core.CategoricalAdapter


-- Regular category theory (4 assertions)

regularCategoryDeclarationAdapt : A.RegularCategoryDeclarationAdapter
regularCategoryDeclarationAdapt = A.mkRegularCategoryDeclarationAdapter _ _ _ refl refl

_ : A.isFilledRegularCategoryDeclaration regularCategoryDeclarationAdapt ≡ true
_ = refl

-- Categorical assertions for RegularCategoryDeclaration
_ : CategoricalAdapter.morphism (A.regularCategoryDeclarationCategorical regularCategoryDeclarationAdapt) ⊤ ⊤ ≡
    CategoricalAdapter.object (A.regularCategoryDeclarationCategorical regularCategoryDeclarationAdapt) ⊤
_ = refl

_ : CategoricalAdapter.isomorphism (A.regularCategoryDeclarationCategorical regularCategoryDeclarationAdapt) ⊤ ⊤ ≡ refl
_ = refl

kernelPairDeclarationAdapt : A.KernelPairDeclarationAdapter
kernelPairDeclarationAdapt = A.mkKernelPairDeclarationAdapter _ _ refl

_ : A.isFilledKernelPairDeclaration kernelPairDeclarationAdapt ≡ true
_ = refl

-- Categorical assertions for KernelPairDeclaration
_ : CategoricalAdapter.morphism (A.kernelPairDeclarationCategorical kernelPairDeclarationAdapt) ⊤ ⊤ ≡
    CategoricalAdapter.object (A.kernelPairDeclarationCategorical kernelPairDeclarationAdapt) ⊤
_ = refl

_ : CategoricalAdapter.isomorphism (A.kernelPairDeclarationCategorical kernelPairDeclarationAdapt) ⊤ ⊤ ≡ refl
_ = refl

internalEquivalenceRelationDeclarationAdapt : A.InternalEquivalenceRelationDeclarationAdapter
internalEquivalenceRelationDeclarationAdapt = A.mkInternalEquivalenceRelationDeclarationAdapter _ _ refl

_ : A.isFilledInternalEquivalenceRelationDeclaration internalEquivalenceRelationDeclarationAdapt ≡ true
_ = refl

-- Categorical assertions for InternalEquivalenceRelationDeclaration
_ : CategoricalAdapter.morphism (A.internalEquivalenceRelationDeclarationCategorical internalEquivalenceRelationDeclarationAdapt) ⊤ ⊤ ≡
    CategoricalAdapter.object (A.internalEquivalenceRelationDeclarationCategorical internalEquivalenceRelationDeclarationAdapt) ⊤
_ = refl

_ : CategoricalAdapter.isomorphism (A.internalEquivalenceRelationDeclarationCategorical internalEquivalenceRelationDeclarationAdapt) ⊤ ⊤ ≡ refl
_ = refl

exactCategoryDeclarationAdapt : A.ExactCategoryDeclarationAdapter
exactCategoryDeclarationAdapt = A.mkExactCategoryDeclarationAdapter _ _ refl

_ : A.isFilledExactCategoryDeclaration exactCategoryDeclarationAdapt ≡ true
_ = refl

-- Categorical assertions for ExactCategoryDeclaration
_ : CategoricalAdapter.morphism (A.exactCategoryDeclarationCategorical exactCategoryDeclarationAdapt) ⊤ ⊤ ≡
    CategoricalAdapter.object (A.exactCategoryDeclarationCategorical exactCategoryDeclarationAdapt) ⊤
_ = refl

_ : CategoricalAdapter.isomorphism (A.exactCategoryDeclarationCategorical exactCategoryDeclarationAdapt) ⊤ ⊤ ≡ refl
_ = refl
