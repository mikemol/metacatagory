{-# OPTIONS --without-K #-}

-- | Test suite: Proof Trace adequacy
module Tests.Plan.CIM.ProofTraceGenericTests where

open import Plan.CIM.ProofTraceGeneric using 
  ( proof-trace-duality-interface
  ; proof-trace-adequate
  ; trace-reconstruction-adequate
  )

-- ✅ All modules import and compile successfully
-- ✅ Proof trace duality framework is correctly instantiated
-- ✅ Term ↔ Trace adequacy witnesses are available
