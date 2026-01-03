{-# OPTIONS --without-K #-}

-- | Example: FunctionPathCategory adequacy hook in action.

module Examples.FunctionPathAdequacy where

open import Agda.Primitive using (Level; lzero)
open import Agda.Builtin.Nat using (Nat; suc)
open import Agda.Builtin.Equality using (_≡_; refl)

open import Infrastructure.Functor.Instances.FunctionPathCategory
open import Infrastructure.Axiom.Instance using (FramedFace; AxiomInstance)
open import Infrastructure.Axiom.Face as Face using (Face)

module FP = FunctionPathAdequacy {ℓ = lzero}

exampleKit : FP.FunctionPathKit
FP.FunctionPathKit.A exampleKit = Nat
FP.FunctionPathKit.B exampleKit = Nat
FP.FunctionPathKit.f exampleKit = lift (λ n → suc n)

exampleFace : FramedFace FP.functionPathAlgebra
exampleFace = FP.functionPathFace exampleKit

exampleSolve : Face.lhs (FramedFace.face exampleFace) ≡ Face.rhs (FramedFace.face exampleFace)
exampleSolve = AxiomInstance.solve FP.functionPathAxiomInstance exampleKit
