-- Tests.YonedaChecklist: Test instances for Yoneda lemma

module Tests.YonedaChecklist where

open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.Equality using (refl)
open import Tests.ObligationAdapters as A

-- Yoneda lemma (2 assertions)

internalYonedaEmbeddingAdapt : A.InternalYonedaEmbeddingAdapter
internalYonedaEmbeddingAdapt = A.mkInternalYonedaEmbeddingAdapter _ _ refl

_ : A.isFilledInternalYonedaEmbedding internalYonedaEmbeddingAdapt ≡ true
_ = refl

internalYonedaLemmaAdapt : A.InternalYonedaLemmaAdapter
internalYonedaLemmaAdapt = A.mkInternalYonedaLemmaAdapter _ _ _ refl refl

_ : A.isFilledInternalYonedaLemma internalYonedaLemmaAdapt ≡ true
_ = refl
