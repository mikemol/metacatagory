{-# OPTIONS --without-K #-}

-- | Examples over transformation systems and their path categories.
module Examples.TransformationSystemExamples where

open import Agda.Primitive using (Level; lzero)
open import Agda.Builtin.Nat using (Nat; zero; suc; _+_)
open import Agda.Builtin.Equality using (_≡_; refl)

open import Plan.CIM.Utility using (TransformationSystem; Path; refl-path; trans-step)
open import Infrastructure.Functor.Interface using (FunctorInstance)
open import Infrastructure.Functor.Instances.TransformationSystem

-- A tiny transformation system over Nat with unit-cost steps.
NatSteps : TransformationSystem {ℓ = lzero} Nat Nat
TransformationSystem.Step NatSteps = Nat
TransformationSystem.cost NatSteps _ = 1

-- Two sample paths and their concatenation via the path category.
stepA : Path NatSteps
stepA = trans-step 1 refl-path

stepB : Path NatSteps
stepB = trans-step 2 (trans-step 3 refl-path)

concatExample : _⊕_ {Sys = NatSteps} stepA stepB ≡ trans-step 1 (trans-step 2 (trans-step 3 refl-path))
concatExample = refl

-- The path identity functor is pointwise identity.
pathIdentityExample : FunctorInstance.map (pathIdentity NatSteps) stepB ≡ stepB
pathIdentityExample = refl
