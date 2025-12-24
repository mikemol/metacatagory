{-# OPTIONS --without-K --cubical-compatible --safe #-}

module Plan.CIM.DocumentSynthesis where

open import Agda.Builtin.String using (String; primStringAppend; primStringEquality)
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.Nat using (Nat; zero; suc)
open import Agda.Builtin.Char using (Char)

open import Plan.CIM.Utility using (_++_; map)
open import Plan.CIM.FrameworkMetadata
open import Plan.CIM.PandocAST

------------------------------------------------------------------------
-- Helper: concatenate lists
------------------------------------------------------------------------

concat : ∀ {ℓ} {A : Set ℓ} → List (List A) → List A
concat [] = []
concat (xs ∷ xss) = xs ++ˡ concat xss
  where
    infixl 1 _++ˡ_
    _++ˡ_ : ∀ {ℓ} {A : Set ℓ} → List A → List A → List A
    [] ++ˡ ys = ys
    (x ∷ xs) ++ˡ ys = x ∷ (xs ++ˡ ys)

------------------------------------------------------------------------
-- Helper functions for document synthesis
------------------------------------------------------------------------

frameToBlocks : Framework → List Block
frameToBlocks fw = 
  Header 3 (Str (Framework.emoji fw ++ " " ++ Framework.name fw) ∷ []) ∷
  Plain (Str "**Primary Theory:** " ∷ Str (Framework.primaryTheory fw) ∷ []) ∷
  Plain (Str "**Key Modules:**" ∷ []) ∷
  BulletList (map (λ m → Plain (Str m ∷ []) ∷ []) (Framework.keyModules fw)) ∷
  Plain (Str "**Provides:**" ∷ []) ∷
  BulletList (map (λ p → Plain (Str p ∷ []) ∷ []) (Framework.provides fw)) ∷
  Plain (Str "**Primary Goal:** " ∷ Str (Framework.primaryGoal fw) ∷ []) ∷
  Plain (Str (Framework.description fw) ∷ []) ∷
  []

concatBlocks : List (List Block) → List Block
concatBlocks = concat

infixl 1 _++ℓ_
_++ℓ_ : List Block → List Block → List Block
[] ++ℓ ys = ys
(x ∷ xs) ++ℓ ys = x ∷ (xs ++ℓ ys)

interfaceToBlocks : Nat → InterfaceBoundary → List Block
interfaceToBlocks level ib =
  Header level (Str (InterfaceBoundary.name ib) ∷ []) ∷
  Plain (Str (InterfaceBoundary.description ib) ∷ []) ∷
  Plain (Str "From: " ∷ Code (InterfaceBoundary.mechanismStart ib) ∷ []) ∷
  Plain (Str "To: " ∷ Code (InterfaceBoundary.mechanismEnd ib) ∷ []) ∷
  []

flexibilityToBlocks : FlexibilityPoint → List Block
flexibilityToBlocks fp =
  Header 3 (Str (FlexibilityPoint.name fp) ∷ []) ∷
  Plain (Str (FlexibilityPoint.description fp) ∷ []) ∷
  Plain (Str "- " ∷ Strong (Str (FlexibilityPoint.alternativeOne fp) ∷ []) ∷ []) ∷
  Plain (Str "- " ∷ Strong (Str (FlexibilityPoint.alternativeTwo fp) ∷ []) ∷ []) ∷
  Plain (Str "**Rationale:** " ∷ Str (FlexibilityPoint.rationale fp) ∷ []) ∷
  []

qualityToBlocks : QualityMandate → List Block
qualityToBlocks qm =
  Header 3 (Str (QualityMandate.name qm ++ " (" ++ QualityMandate.abbreviation qm ++ ")") ∷ []) ∷
  Plain (Str (QualityMandate.description qm) ∷ []) ∷
  Plain (Str "Examples:" ∷ []) ∷
  BulletList (map (λ ex → Plain (Str ex ∷ []) ∷ []) (QualityMandate.examples qm)) ∷
  []

violationToBlocks : A12ProtocolViolation → List Block
violationToBlocks v =
  Header 3 (Str (A12ProtocolViolation.violationType v) ∷ []) ∷
  Plain (Str (A12ProtocolViolation.description v) ∷ []) ∷
  Plain (Str "Detection: " ∷ Code (A12ProtocolViolation.detection v) ∷ []) ∷
  Plain (Str "Correction: " ∷ Str (A12ProtocolViolation.correction v) ∷ []) ∷
  []

------------------------------------------------------------------------
-- Framework Interoperability Document
------------------------------------------------------------------------

frameworkInteroperabilityDoc : PandocDoc
frameworkInteroperabilityDoc = record
  { blocks =
      Header topLevel (Str "Framework Interoperability Map" ∷ []) ∷
      Plain (Str "**Generated:** December 21, 2025" ∷ []) ∷
      Plain (Str "**Purpose:** Document how multiple theoretical frameworks compose within the metacatagory repository" ∷ []) ∷
      HorizontalRule ∷
      Header sectionLevel (Str "Executive Summary" ∷ []) ∷
      Para (Str "The metacatagory repository is **not a monolithic system** but a **composable substrate** where multiple theoretical frameworks interoperate through well-defined interfaces." ∷ []) ∷
      BulletList (
        (Plain (Str "✅ **Parallel development** of categorical and geometric theories without blocking each other" ∷ []) ∷ []) ∷
        (Plain (Str "✅ **Flexible interpretation** - same abstraction can have multiple theoretical justifications" ∷ []) ∷ []) ∷
        (Plain (Str "✅ **Pragmatic composition** - use what works without requiring full unification" ∷ []) ∷ []) ∷
        (Plain (Str "✅ **Framework isolation** - changes in one theory don't cascade to others" ∷ []) ∷ []) ∷
        []
      ) ∷
      HorizontalRule ∷
      Header sectionLevel (Str "Part I: Framework Inventory" ∷ []) ∷
      concatBlocks (map frameToBlocks allFrameworks) ++ℓ
      (HorizontalRule ∷
      Header sectionLevel (Str "Part II: Interface Boundaries" ∷ []) ∷
      Plain (Str "The following interface boundaries define how frameworks interact and compose:" ∷ []) ∷
      concatBlocks (map (interfaceToBlocks subSectionLevel) interfaceBoundaries) ++ℓ
      (HorizontalRule ∷
      Header sectionLevel (Str "Part III: Intentional Flexibility Points" ∷ []) ∷
      Plain (Str "The system contains explicit flexibility points where alternative implementations are supported:" ∷ []) ∷
      concatBlocks (map flexibilityToBlocks flexibilityPoints) ++ℓ
      []))
  ; meta = ""
  }
  where
    topLevel : Nat
    topLevel = 1

    sectionLevel : Nat
    sectionLevel = suc topLevel

    subSectionLevel : Nat
    subSectionLevel = suc sectionLevel

------------------------------------------------------------------------
-- Quality Framework Document
------------------------------------------------------------------------

qualityFrameworkDoc : PandocDoc
qualityFrameworkDoc = record
  { blocks =
      Header 1 (Str "Quality Mandate Framework" ∷ []) ∷
      Plain (Str "**Generated:** December 21, 2025" ∷ []) ∷
      Plain (Str "**Purpose:** Formalize the six quality mandates that govern all CIM implementations" ∷ []) ∷
      Header 2 (Str "The Quality Mandate Sextet" ∷ []) ∷
      Para (Str "Every component in the metacatagory system must satisfy all six mandates:" ∷ []) ∷
      concatBlocks (map qualityToBlocks qualityMandates) ++ℓ
      (HorizontalRule ∷
      Header 2 (Str "A12 Correction Protocol" ∷ []) ∷
      Plain (Str "The A12 protocol identifies and corrects violations of the quality mandates:" ∷ []) ∷
      concatBlocks (map violationToBlocks a12Violations) ++ℓ
      [])
  ; meta = ""
  }
