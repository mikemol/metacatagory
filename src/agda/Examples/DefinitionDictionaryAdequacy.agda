{-# OPTIONS --without-K #-}

-- | Example: Using DefinitionDictionary's adequacy instance to solve
-- declarative faces.  Demonstrates lookup plus the framed-face/solve bridge.

module Examples.DefinitionDictionaryAdequacy where

open import Agda.Builtin.Equality using (_≡_)
open import Agda.Builtin.Unit using (⊤; tt)
open import Agda.Builtin.Maybe using (Maybe; just; nothing)
open import Agda.Builtin.String using (String)

open import Infrastructure.Definitions.Dictionary
open import Infrastructure.Axiom.Instance using (AxiomInstance; FramedFace)
open import Infrastructure.Axiom.Face using (Face)

-- A small local dictionary entry to keep the example self contained.
exampleEntry : DefinitionEntry
exampleEntry = record
  { term       = "ExampleTerm"
  ; summary    = "Toy definition used to exercise the adequacy bridge."
  ; provenance = ("Examples/DefinitionDictionaryAdequacy.agda" ∷ [])
  }

-- Lookup succeeds on our synthetic dictionary.
exampleLookup : lookupDefinition "ExampleTerm" (exampleEntry ∷ definitions) ≡ just exampleEntry
exampleLookup = refl

-- The adequacy instance converts an entry into a solvable face with identical boundaries.
exampleFace : FramedFace definitionAlgebra
exampleFace = entryFace exampleEntry

exampleSolve : Face.lhs (FramedFace.face exampleFace) ≡ Face.rhs (FramedFace.face exampleFace)
exampleSolve = AxiomInstance.solve definitionInstance exampleEntry
