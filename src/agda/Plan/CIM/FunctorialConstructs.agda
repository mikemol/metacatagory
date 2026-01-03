{-# OPTIONS --without-K --cubical-compatible #-}

-- | Functorial and coherence constructs factored out of Utility.
module Plan.CIM.FunctorialConstructs where

open import Agda.Primitive using (Level; lsuc)
open import Agda.Builtin.String using (String)
open import Plan.CIM.Metricization using (EmergentMetric; mkMetric; NonZero; nonZero?) public
open import Plan.CIM.TransformationSystem using (PhaseAmbiguity; TransformationSystem; Path; _×_) public

-- | Witness of coherence with an explicit proof path and metric.
record CoherenceWitness {ℓ} {A B : Set ℓ} (amb : PhaseAmbiguity {ℓ} A B) (Sys : TransformationSystem {ℓ} A B) : Set (lsuc ℓ) where
    field
        proofPath : Path Sys
        metric    : EmergentMetric

-- | Braided inheritance functor with swap and cost.
record BraidedInheritanceFunctor {ℓ} (A B : Set ℓ) : Set (lsuc ℓ) where
    field
        inheritanceBraid : (A × B) → (B × A)
        coherenceCost    : EmergentMetric
        fromValue : A
        toValue   : B
        description : String

-- | Packed SPPF node carrying left/right paths and a braid resolution.
record BraidedSPPF {ℓ} (A B : Set ℓ) (Sys : TransformationSystem A B) : Set (lsuc ℓ) where
    constructor packed-node
    field
        leftPath   : Path Sys
        rightPath  : Path Sys
        resolution : BraidedInheritanceFunctor A B

-- | Compact normal form protocol combining ambiguity, system, and coherence.
record CNFProtocol (A B : Set) : Set₁ where
    field
        ambiguity : PhaseAmbiguity A B
        transSys  : TransformationSystem A B
        coherence : CoherenceWitness ambiguity transSys
        metric    : EmergentMetric
