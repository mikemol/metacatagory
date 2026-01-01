-- Unified Roadmap Index
-- Re-exports all roadmap categories

module Plan.CIM.IngestedRoadmaps where

open import Plan.CIM.IngestedRoadmaps.Foundation renaming (roadmapsV2 to roadmapsFoundation) public
open import Plan.CIM.IngestedRoadmaps.Geometry renaming (roadmapsV2 to roadmapsGeometry) public
open import Plan.CIM.IngestedRoadmaps.Corrections renaming (roadmapsV2 to roadmapsCorrections) public
open import Plan.CIM.IngestedRoadmaps.Polytopes renaming (roadmapsV2 to roadmapsPolytopes) public
open import Plan.CIM.IngestedRoadmaps.Analysis renaming (roadmapsV2 to roadmapsAnalysis) public

open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.String using (String)
open import Plan.CIM.Utility using (RoadmapStepV2)

infixr 5 _++ˡ_
_++ˡ_ : ∀ {ℓ} {A : Set ℓ} → List A → List A → List A
[] ++ˡ ys = ys
(x ∷ xs) ++ˡ ys = x ∷ (xs ++ˡ ys)

-- Aggregated enriched roadmap list (currently empty placeholders)
allRoadmapsV2 : List RoadmapStepV2
allRoadmapsV2 = roadmapsFoundation
               ++ˡ roadmapsGeometry
               ++ˡ roadmapsCorrections
               ++ˡ roadmapsPolytopes
               ++ˡ roadmapsAnalysis

------------------------------------------------------------------------
-- Cohomology / example-preservation checklist hook
------------------------------------------------------------------------

-- Roadmaps should capture explicit example coverage to prevent capability drift.
-- This sentinel can be queried/extended by downstream exporters/pipelines.
cohomologyChecklistNote : String
cohomologyChecklistNote =
  "Each roadmap task must preserve cohomological coverage: add or update an explicit example in src/agda/Examples for every new capability or refactor that changes usage patterns."
