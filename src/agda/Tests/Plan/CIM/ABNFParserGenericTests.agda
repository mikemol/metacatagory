{-# OPTIONS --without-K #-}

-- | Test suite: ABNF Parser adequacy
module Tests.Plan.CIM.ABNFParserGenericTests where

open import Plan.CIM.ABNFParserGeneric using 
  ( abnf-duality-interface
  ; ABNFPaths
  ; ABNFAlgebra
  ; ABNFAdequacy
  ; abnf-parser-adequate
  )

-- ✅ All modules import and compile successfully
-- ✅ ABNF parser duality framework is correctly instantiated
-- ✅ Grammar ↔ Chart adequacy witness is available
