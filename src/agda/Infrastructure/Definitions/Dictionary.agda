{-# OPTIONS --without-K #-}

-- | DefinitionDictionary: canonical store for semantic definitions paired with
-- trivial solvable interfaces.  Each entry names a term, gives a summary, and
-- records provenance.  The accompanying ``AxiomInstance`` provides a bridge to
-- the adequacy layer so downstream modules can treat definition coverage as a
-- solvable boundary.

module Infrastructure.Definitions.Dictionary where

open import Agda.Primitive using (Level; lzero)
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.String using (String; primStringEquality)
open import Agda.Builtin.Maybe using (Maybe; just; nothing)
open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.Equality using (_≡_; refl)
open import Agda.Builtin.Unit using (⊤; tt)

open import Infrastructure.Axiom.Adequacy using (PathAlgebra)
open import Infrastructure.Axiom.Face using (Face)
open import Infrastructure.Axiom.Instance using (AxiomInstance; FramedFace)

------------------------------------------------------------------------
-- Definition entries
------------------------------------------------------------------------

-- | Single dictionary entry pairing a term with summary and provenance.
record DefinitionEntry : Set where
  field
    term       : String
    summary    : String
    provenance : List String

-- | Facet metadata for grouping related definitions.
record DefinitionFacet : Set where
  field
    name        : String
    description : String
    related     : List String

------------------------------------------------------------------------
-- Lookup and faces
------------------------------------------------------------------------

lookupDefinition : String → List DefinitionEntry → Maybe DefinitionEntry
lookupDefinition _ [] = nothing
lookupDefinition t (d ∷ ds) with primStringEquality t (DefinitionEntry.term d)
... | true  = just d
... | false = lookupDefinition t ds

-- | Trivial path algebra: every pair of terms has a unique unit path.
definitionAlgebra : PathAlgebra {ℓV = lzero} {ℓP = lzero} String
PathAlgebra.Path definitionAlgebra _ _ = ⊤
PathAlgebra._++_ definitionAlgebra _ _ = tt
PathAlgebra.++-assoc definitionAlgebra _ _ _ = refl
PathAlgebra.id definitionAlgebra {a = _} = tt
PathAlgebra.id-left definitionAlgebra {a = _} {b = _} p = refl
PathAlgebra.id-right definitionAlgebra {a = _} {b = _} p = refl

-- | Package an entry as a framed face with identical endpoints and unit paths.
entryFace : DefinitionEntry → FramedFace definitionAlgebra
FramedFace.a    (entryFace e) = DefinitionEntry.term e
FramedFace.b    (entryFace e) = DefinitionEntry.term e
FramedFace.face (entryFace e) = record { lhs = tt ; rhs = tt }

faces : List DefinitionEntry → List (FramedFace definitionAlgebra)
faces [] = []
faces (d ∷ ds) = entryFace d ∷ faces ds

-- | Axiom instance witnessing that each definition entry is solvable.
definitionInstance : AxiomInstance definitionAlgebra
AxiomInstance.Kit   definitionInstance = DefinitionEntry
AxiomInstance.face  definitionInstance = entryFace
AxiomInstance.solve definitionInstance _ = refl

-- | Seed definitions drawn from the homological audit.  These can be extended
-- as semantic coverage grows.
definitions : List DefinitionEntry
definitions =
  record
    { term       = "DefinitionDictionary"
    ; summary    = "Canonical dictionary closing the semantic/proof completeness loop."
    ; provenance = ("intake/codex_handoff.md" ∷ [])
    }
  ∷
  record
    { term       = "GenericFunctorInterface"
    ; summary    = "Generic functor class ensuring composition and identity laws hold uniformly."
    ; provenance = ("intake/codex_handoff.md" ∷ [])
    }
  ∷
  record
    { term       = "TriangulationKit"
    ; summary    = "Required coherence data for new parsers; supplies pentagon/hexagon fillers."
    ; provenance = ("intake/codex_handoff.md" ∷ [])
    }
  ∷
  record
    { term       = "FunctorInterface"
    ; summary    = "Generic functor interface to enforce identity and composition laws across protocol bundles."
    ; provenance = ("src/agda/Infrastructure/Functor/Interface.agda" ∷ [])
    }
  ∷
  record
    { term       = "SolverAdequacy"
    ; summary    = "Every axiomatic boundary must be backed by a generator kit and solver rather than postulated."
    ; provenance = ("intake/codex_handoff.md" ∷ "src/agda/Infrastructure/Axiom/Instance.agda" ∷ [])
    }
  ∷
  []
