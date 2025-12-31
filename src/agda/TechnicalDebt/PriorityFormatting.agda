open import Agda.Builtin.Int using (Int)
open import Agda.Builtin.String using (String; primStringAppend)

module TechnicalDebt.PriorityFormatting (intToString : Int → String) where

concatStr : String → String → String
concatStr = primStringAppend

open import TechnicalDebt.DeferredItemsFormatting using (mkListLike; AUDAXBlock; AUDAXDoc; AUDAXInline; ListLike)
open import TechnicalDebt.PriorityMapping using (CategoryWeights; strategyToWeights)
open import Agda.Builtin.List using (List; []; _∷_)
open import TechnicalDebt.Priorities using (PriorityStrategy)
open import Agda.Primitive using (Level; _⊔_)
open import Core.Phase using (_×_; Σ; _,_)
infixr 2 _×_


map : ∀ {ℓ ℓ'} {A : Set ℓ} {B : Set ℓ'} → (A → B) → List A → List B
map f [] = []
map f (x ∷ xs) = f x ∷ map f xs

weightsAsFields : CategoryWeights → List (String × String)
weightsAsFields w =
  ("postulate" , intToString (CategoryWeights.postulateWeight w)) ∷
  ("todo"      , intToString (CategoryWeights.todoWeight w)) ∷
  ("fixme"     , intToString (CategoryWeights.fixmeWeight w)) ∷
  ("deviation", intToString (CategoryWeights.deviationWeight w)) ∷ []

formatStrategyBlock : (String × PriorityStrategy) → List AUDAXBlock
formatStrategyBlock (n , s) =
  AUDAXBlock.Header 2 (headerBlock) ∷
  AUDAXBlock.Table header rows ∷ []
  where
    w      = strategyToWeights s
    fields = weightsAsFields w
    header : ListLike String
    header = mkListLike ("Category" ∷ "Weight" ∷ [])
    rows : ListLike (ListLike AUDAXInline)
    rows   = mkListLike (map (\(k , v) → mkListLike (AUDAXInline.Str k ∷ AUDAXInline.Str v ∷ [])) fields)
    headerBlock : ListLike AUDAXInline
    headerBlock = mkListLike (AUDAXInline.Str (concatStr n " Strategy") ∷ [])

-- Helper: flatten list of lists
_++_ : ∀ {ℓ} {A : Set ℓ} → List A → List A → List A
[] ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)
concat : ∀ {ℓ} {A : Set ℓ} → List (List A) → List A
concat [] = []
concat (x ∷ xs) = x ++ concat xs


formatAllStrategiesAUDAXDoc : List (String × PriorityStrategy) → AUDAXDoc
formatAllStrategiesAUDAXDoc xs =
  let blocks = concat (map formatStrategyBlock xs)
  in record { blocks = mkListLike blocks; meta = "" }


formatPriorityAUDAXDoc : String → PriorityStrategy → AUDAXDoc
formatPriorityAUDAXDoc name strat =
  let w      = strategyToWeights strat
      fields  = weightsAsFields w
      header : ListLike String
      header   = mkListLike ("Category" ∷ "Weight" ∷ [])
      rows : ListLike (ListLike AUDAXInline)
      rows     = mkListLike (map (\(k , v) → mkListLike (AUDAXInline.Str k ∷ AUDAXInline.Str v ∷ [])) fields)
      blocks : ListLike AUDAXBlock
      blocks   = mkListLike (
        AUDAXBlock.Header 2 (mkListLike (AUDAXInline.Str (concatStr name " Strategy") ∷ [])) ∷
        AUDAXBlock.Table header rows ∷
        [] )
  in record { blocks = blocks; meta = "" }

testStr : String
testStr = concatStr "foo" "bar"
