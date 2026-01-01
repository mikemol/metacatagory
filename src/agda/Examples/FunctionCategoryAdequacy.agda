{-# OPTIONS --without-K #-}

-- | Demonstrate the parameterized adequacy hook for FunctionCategory.
-- We build a simple function kit, show the framed face, and observe that
-- the solver discharges the face by construction.

module Examples.FunctionCategoryAdequacy where

open import Agda.Primitive using (Level; lzero)
open import Agda.Builtin.Nat using (Nat; suc; _+_)
open import Agda.Builtin.Equality using (_≡_; refl)

open import Infrastructure.Functor.Instances.FunctionCategory

-- Instantiate the adequacy module at the base universe.
module FC = FunctionCategoryAdequacy {ℓ = lzero}

-- A simple increment function packed as a FunctionKit.
exampleKit : FC.FunctionKit
FC.FunctionKit.A exampleKit = Nat
FC.FunctionKit.B exampleKit = Nat
FC.FunctionKit.f exampleKit = lift (λ n → suc n)

-- The framed face for the kit; both sides are the same lifted function.
exampleFace : FramedFace FC.functionPathAlgebra
exampleFace = FC.functionFace exampleKit

-- The adequacy solver witnesses lhs ≡ rhs (trivially refl here).
exampleSolution : Face.lhs (FramedFace.face exampleFace) ≡ Face.rhs (FramedFace.face exampleFace)
exampleSolution = AxiomInstance.solve FC.functionAxiomInstance exampleKit
