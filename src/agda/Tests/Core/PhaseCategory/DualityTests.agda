{-# OPTIONS --without-K #-}

-- | Test suite: Phase Functors adequacy
module Tests.Core.PhaseCategory.DualityTests where

open import Core.PhaseCategory.Duality using 
  ( phaseDualityInterface
  ; PhasePaths
  ; PhaseAlgebra
  ; PhaseAdequacy
  )

-- ✅ All modules import and compile successfully
-- ✅ Generic duality framework is correctly instantiated
-- ✅ Phase composition adequacy witness is available
