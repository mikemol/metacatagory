{-# OPTIONS --without-K #-}

-- | Re-export of Face/PathAlgebra to avoid duplicate definitions.
module Infrastructure.Axiom.Face where

-- Re-export Face from Adequacy to avoid duplicate definitions.
open import Infrastructure.Axiom.Adequacy public using (PathAlgebra; Face)
