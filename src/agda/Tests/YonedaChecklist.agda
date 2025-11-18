-- Tests.YonedaChecklist: Test instances for Yoneda lemma

module Tests.YonedaChecklist where

open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.Equality using (refl)
open import Agda.Builtin.Unit using (⊤)
open import Tests.ObligationAdapters as A
open import Core.CategoricalAdapter


-- Yoneda lemma (2 assertions)

internalYonedaEmbeddingAdapt : A.InternalYonedaEmbeddingAdapter
internalYonedaEmbeddingAdapt = A.mkInternalYonedaEmbeddingAdapter _ _ refl

_ : A.isFilledInternalYonedaEmbedding internalYonedaEmbeddingAdapt ≡ true
_ = refl

-- Categorical assertions for InternalYonedaEmbedding
_ : CategoricalAdapter.morphism (A.internalYonedaEmbeddingCategorical internalYonedaEmbeddingAdapt) ⊤ ⊤ ≡
    CategoricalAdapter.object (A.internalYonedaEmbeddingCategorical internalYonedaEmbeddingAdapt) ⊤
_ = refl

_ : CategoricalAdapter.isomorphism (A.internalYonedaEmbeddingCategorical internalYonedaEmbeddingAdapt) ⊤ ⊤ ≡ refl
_ = refl

internalYonedaLemmaAdapt : A.InternalYonedaLemmaAdapter
internalYonedaLemmaAdapt = A.mkInternalYonedaLemmaAdapter _ _ _ refl refl

_ : A.isFilledInternalYonedaLemma internalYonedaLemmaAdapt ≡ true
_ = refl

-- Categorical assertions for InternalYonedaLemma
_ : CategoricalAdapter.morphism (A.internalYonedaLemmaCategorical internalYonedaLemmaAdapt) ⊤ ⊤ ≡
    CategoricalAdapter.object (A.internalYonedaLemmaCategorical internalYonedaLemmaAdapt) ⊤
_ = refl

_ : CategoricalAdapter.isomorphism (A.internalYonedaLemmaCategorical internalYonedaLemmaAdapt) ⊤ ⊤ ≡ refl
_ = refl
