{-# OPTIONS --without-K #-}

-- | Example: TransformationSystem path adequacy hook.

module Examples.TransformationPathAdequacy where

open import Agda.Primitive using (Level; lzero)
open import Agda.Builtin.Nat using (Nat; suc)
open import Agda.Builtin.Equality using (_≡_; refl)

open import Plan.CIM.Utility using (TransformationSystem; Path; refl-path; trans-step)
open import Infrastructure.Functor.Instances.TransformationSystem

NatSteps : TransformationSystem {ℓ = lzero} Nat Nat
TransformationSystem.Step NatSteps = Nat
TransformationSystem.cost NatSteps _ = 1

module TP = TransformationPathAdequacy NatSteps

exampleKit : TP.PathKit
TP.PathKit.p exampleKit = trans-step 1 (trans-step 2 refl-path)

exampleFace : FramedFace TP.pathAlgebra
exampleFace = TP.pathFace exampleKit

exampleSolve : Face.lhs (FramedFace.face exampleFace) ≡ Face.rhs (FramedFace.face exampleFace)
exampleSolve = AxiomInstance.solve TP.pathAxiomInstance exampleKit
