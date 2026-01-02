{-# OPTIONS --without-K #-}

-- | Instantiate field theory parameters with classical theorems.
module Algebra.Fields.ClassicalInstance where

open import Algebra.Fields.Theorems.Classical as Classical
open import Algebra.Fields.Basic
  Classical.degreeOfExtensionFormula
  Classical.fundamentalTheoremGalois
  Classical.galoisIffNormalSeparable
  Classical.fundamentalTheoremAlgebra
  Classical.vectorSpacesOverFields
  Classical.fieldsAreSimpleCommutativeRings
  Classical.functionFieldsAndGalois
  public
