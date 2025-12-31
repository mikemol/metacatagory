{-# OPTIONS --without-K --cubical-compatible #-}

module Plan.CIM.RoadmapExporter where

open import Agda.Builtin.String using (String; primStringAppend)
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.Nat using (Nat; zero; suc; _+_)
open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.Unit using (⊤; tt)
open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.Maybe using (Maybe; just; nothing)

open import Plan.CIM.Utility hiding (_++_; map)
open import Plan.CIM.PandocAST using (PandocDoc; MarkdownDoc; Block; Inline; MdBlock)
open import Plan.CIM.PandocShowInline using (showInlines; showMdInlines)
open import Plan.CIM.PandocShowBlock using (showBlock; showBlocks; showBlockLists)
open import Plan.CIM.PandocShowMdBlock using (showMdBlock; showMdBlocks; showMdBlockLists)
open import Plan.CIM.DocumentationContent
open import Plan.CIM.DocumentSynthesis

postulate
  writeFile : String → String → IO ⊤
  _>>=_ : ∀ {A B : Set} → IO A → (A → IO B) → IO B
  return : ∀ {A : Set} → A → IO A

infixr 20 _++_
_++_ : String → String → String
_++_ = primStringAppend

map : ∀ {ℓ ℓ'} {A : Set ℓ} {B : Set ℓ'} → (A → B) → List A → List B
map f [] = []
map f (x ∷ xs) = f x ∷ map f xs

_++ˡ_ : ∀ {ℓ} {A : Set ℓ} → List A → List A → List A
[] ++ˡ ys = ys
(x ∷ xs) ++ˡ ys = x ∷ (xs ++ˡ ys)

astExportString : PandocDoc → MarkdownDoc → String
astExportString pd md =
  "PANDOC AST:\n" ++ showBlocks (PandocDoc.blocks pd)
  ++ "\n\nMARKDOWN AST:\n" ++ showMdBlocks (MarkdownDoc.blocks md)

exportASTs : PandocDoc → MarkdownDoc → String → IO ⊤
exportASTs pd md path =
  let outStr = astExportString pd md
  in writeFile path outStr >>= λ _ → return tt
