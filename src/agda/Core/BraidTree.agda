{-# OPTIONS --without-K #-}

module Core.BraidTree where

open import Agda.Builtin.String using (String; primStringAppend)
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.Maybe using (Maybe; nothing)
open import Agda.Builtin.Nat using (Nat; zero; suc; _+_)
open import Agda.Builtin.Sigma using (Σ; _,_; fst; snd)

open import Plan.CIM.TransformationSystem using
  (TransformationSystem; Path; refl-path; trans-step; PhaseAmbiguity)
open import Plan.CIM.FunctorialConstructs using
  (BraidedInheritanceFunctor; BraidedSPPF; EmergentMetric; packed-node)
open import Plan.CIM.Metricization using (mkMetric)
open import Plan.CIM.RoadmapSPPF using (SPPFNode)

_++_ : String → String → String
_++_ = primStringAppend

infixl 6 _++_

append : ∀ {A : Set} → List A → List A → List A
append [] ys       = ys
append (x ∷ xs) ys = x ∷ append xs ys

-- | Fundamental 0-cell representation in the braid engine.
record PropertyNode : Set where
  field
    identifier : String
    title      : String
    descriptor : String

mkPropertyNode : String → String → String → PropertyNode
mkPropertyNode ident title descriptor =
  record { identifier = ident ; title = title ; descriptor = descriptor }

-- | Convert a property node into the SPPF leaf that seeds the tree.
propertyToSPPF : PropertyNode → SPPFNode
propertyToSPPF node =
  record
    { nodeId   = PropertyNode.identifier node
    ; title    = PropertyNode.title node
    ; status   = "braid-leaf"
    ; category = "Core.BraidTree"
    ; source   = PropertyNode.descriptor node
    ; files    = []
    ; tags     = []
    ; parent1  = nothing
    ; parent2  = nothing
    }

-- | Transformation system that stitches SPPF nodes for the braid engine.
SPPFStep : Set
SPPFStep = String

braidSystem : TransformationSystem SPPFNode SPPFNode
braidSystem = record { Step = SPPFStep ; cost = λ _ → suc zero }

DerivationPath : Set
DerivationPath = Path braidSystem

pathFromLabels : List String → DerivationPath
pathFromLabels []        = refl-path
pathFromLabels (l ∷ xs) = trans-step l (pathFromLabels xs)

pathLength : DerivationPath → Nat
pathLength refl-path          = zero
pathLength (trans-step _ rest) = suc (pathLength rest)

metricFromPath : DerivationPath → EmergentMetric
metricFromPath path = mkMetric (pathLength path) (pathLength path)

metricFromPaths : DerivationPath → DerivationPath → EmergentMetric
metricFromPaths p q = mkMetric (pathLength p) (pathLength q)

swapPair : ∀ {A B : Set} → Σ A (λ _ → B) → Σ B (λ _ → A)
swapPair pair = snd pair , fst pair

-- | Braid functor that swaps the pair while annotating a metric.
mkFunctor
  : String
  → SPPFNode
  → SPPFNode
  → EmergentMetric
  → BraidedInheritanceFunctor SPPFNode SPPFNode
mkFunctor desc left right metric = record
  { inheritanceBraid = swapPair
  ; coherenceCost    = metric
  ; fromValue        = left
  ; toValue          = right
  ; description      = desc
  }

mkBraidEvent
  : String
  → SPPFNode
  → SPPFNode
  → DerivationPath
  → DerivationPath
  → BraidedSPPF SPPFNode SPPFNode braidSystem
mkBraidEvent desc left right leftPath rightPath =
  packed-node leftPath rightPath (mkFunctor desc left right (metricFromPaths leftPath rightPath))

-- | Decorated braid node that captures the synthesis event.
record BraidNode : Set₁ where
  field
    identifier : String
    synopsis   : String
    event      : BraidedSPPF SPPFNode SPPFNode braidSystem

-- | Recursive braid tree over SPPF leaves and synthesis events.
data BraidTree : Set₁ where
  braidLeaf   : SPPFNode → BraidTree
  braidBranch : BraidNode → BraidTree → BraidTree → BraidTree

braidTreeFromEvent
  : String
  → String
  → BraidedSPPF SPPFNode SPPFNode braidSystem
  → BraidTree
braidTreeFromEvent ident synopsis event =
  let functor = BraidedSPPF.resolution event
      left    = braidLeaf (BraidedInheritanceFunctor.fromValue functor)
      right   = braidLeaf (BraidedInheritanceFunctor.toValue functor)
  in braidBranch (record { identifier = ident
                         ; synopsis   = synopsis
                         ; event      = event
                         })
                  left
                  right

-- | Collect all coherence metrics stored in a braid tree.
braidMetrics : BraidTree → List EmergentMetric
braidMetrics (braidLeaf _) = []
braidMetrics (braidBranch node left right) =
  BraidedInheritanceFunctor.coherenceCost (BraidedSPPF.resolution (BraidNode.event node))
    ∷ append (braidMetrics left) (braidMetrics right)

-- | Synthesized property for a Hom-set between two nodes.
homPropertyNode : PropertyNode → PropertyNode → PropertyNode
homPropertyNode dom cod =
  let domId    = PropertyNode.identifier dom
      codId    = PropertyNode.identifier cod
      domTitle = PropertyNode.title dom
      codTitle = PropertyNode.title cod
  in mkPropertyNode (domId ++ ".hom." ++ codId)
                   ("Hom(" ++ domTitle ++ "," ++ codTitle ++ ")")
                   ("Induced Hom set between " ++ domTitle ++ " and " ++ codTitle)

-- | Synthesized property for a categorical pair.
pairPropertyNode : PropertyNode → PropertyNode → PropertyNode
pairPropertyNode a b =
  let aId    = PropertyNode.identifier a
      bId    = PropertyNode.identifier b
      aTitle = PropertyNode.title a
      bTitle = PropertyNode.title b
  in mkPropertyNode (aId ++ ".×." ++ bId)
                   ("Pair(" ++ aTitle ++ "," ++ bTitle ++ ")")
                   ("Packed pair of " ++ aTitle ++ " and " ++ bTitle)

-- | Human-readable description of the braided core.
braidCoreSynopsis : String
braidCoreSynopsis =
  "Core.BraidTree realizes CHIP braid events for Hom sets and " ++
  "Adjunctions using SPPF leaves, metrics, and functorial resolution."

-- | Build the SPPF event representing Hom(A, B) ≅ Hom(B, A).
categoryOfMorphismsSPPF
  : PropertyNode → PropertyNode
  → BraidedSPPF SPPFNode SPPFNode braidSystem
categoryOfMorphismsSPPF a b =
  mkBraidEvent
    ("Hom-set braid for " ++ PropertyNode.identifier a ++ " and " ++ PropertyNode.identifier b)
    (propertyToSPPF (homPropertyNode a b))
    (propertyToSPPF (homPropertyNode b a))
    (pathFromLabels (PropertyNode.identifier a ∷ PropertyNode.identifier b ∷ "hom-left" ∷ []))
    (pathFromLabels (PropertyNode.identifier b ∷ PropertyNode.identifier a ∷ "hom-right" ∷ []))

-- | Build the SPPF event that resolves the adjacency of Hom(F(A), B) and Hom(A, G(B)).
adjunctionStructureSPPF
  : PropertyNode → PropertyNode → PropertyNode → PropertyNode
  → BraidedSPPF SPPFNode SPPFNode braidSystem
adjunctionStructureSPPF fA b a gB =
  mkBraidEvent
    "Adjunction braid"
    (propertyToSPPF (homPropertyNode fA b))
    (propertyToSPPF (homPropertyNode a gB))
    (pathFromLabels (PropertyNode.identifier fA ∷ PropertyNode.identifier b ∷ "adjunction-left" ∷ []))
    (pathFromLabels (PropertyNode.identifier a ∷ PropertyNode.identifier gB ∷ "adjunction-right" ∷ []))

-- | Hom-set braid tree rooted at the synthesized Hom event.
categoryOfMorphismsTree : PropertyNode → PropertyNode → BraidTree
categoryOfMorphismsTree a b =
  braidTreeFromEvent
    ("Hom:" ++ PropertyNode.identifier a ++ "," ++ PropertyNode.identifier b)
    ("Braids the Hom set between " ++ PropertyNode.title a ++ " and " ++ PropertyNode.title b)
    (categoryOfMorphismsSPPF a b)

-- | Adjunction braid tree handling the natural isomorphism between Hom(F(A), B) and Hom(A, G(B)).
adjunctionTree : PropertyNode → PropertyNode → PropertyNode → PropertyNode → BraidTree
adjunctionTree fA b a gB =
  braidTreeFromEvent
    ("Adjunction:" ++ PropertyNode.identifier fA ++ "." ++ PropertyNode.identifier gB)
    "Braids the adjunction between F and G"
    (adjunctionStructureSPPF fA b a gB)

-- | Phase ambiguity witness for a property pair.
homPhaseAmbiguity : PropertyNode → PropertyNode → PhaseAmbiguity SPPFNode SPPFNode
homPhaseAmbiguity a b =
  record
    { valA  = propertyToSPPF a
    ; valB  = propertyToSPPF b
    ; phase = pathLength (pathFromLabels (PropertyNode.identifier a ∷ PropertyNode.identifier b ∷ []))
    }
