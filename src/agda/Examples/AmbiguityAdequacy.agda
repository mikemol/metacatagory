{-# OPTIONS --without-K #-}

-- | Example: Ambiguity functor adequacy hook (parameterized by Funext).

module Examples.AmbiguityAdequacy where

open import Agda.Primitive using (Level; lzero)
open import Agda.Builtin.Bool using (Bool)
open import Agda.Builtin.Equality using (_≡_; refl)

open import Infrastructure.Functor.Instances.Ambiguity
open import Infrastructure.Functor.Instances.FunctionCategory using (Lift; lift)
open import Infrastructure.Functor.Adapters.Funext
open import Infrastructure.Axiom.Instance using (AxiomInstance; FramedFace)
open import Infrastructure.Axiom.Face using (Face)
open import Plan.CIM.Ambiguity using (Ambiguity; determinate)

-- Simple function extensionality assumption packaged as a kit.
postulate funextAny : ∀ {A B : Set lzero} → Funext (Ambiguity A) (Ambiguity B)

module AA = AmbiguityAdequacy {ℓ = lzero} (λ {A} {B} → funextAny)

exampleKit : AA.AmbiguityKit
AA.AmbiguityKit.A exampleKit = Bool
AA.AmbiguityKit.B exampleKit = Bool
AA.AmbiguityKit.f exampleKit = lift (λ x → x)

exampleFace : FramedFace AA.ambiguityPathAlgebra
exampleFace = AA.ambiguityFace exampleKit

exampleSolve : Face.lhs (FramedFace.face exampleFace) ≡ Face.rhs (FramedFace.face exampleFace)
exampleSolve = AxiomInstance.solve AA.ambiguityAxiomInstance exampleKit
