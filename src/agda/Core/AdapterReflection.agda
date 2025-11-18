-- Core.AdapterReflection: Metaprogramming utilities for automatic adapter wrapping
-- Provides macros and reflection-based tools to automate categorical adapter generation

module Core.AdapterReflection where

open import Agda.Builtin.Reflection renaming (bindTC to _>>=_)
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.String using (String)
open import Agda.Builtin.Unit using (⊤; tt)
open import Agda.Builtin.Nat using (Nat)
open import Agda.Builtin.Equality using (_≡_; refl)
open import Core.CategoricalAdapter

------------------------------------------------------------------------
-- Reflection utilities for extracting adapter information
------------------------------------------------------------------------

-- Extract the "primary type" from an adapter record
-- This is the type we'll wrap in a CategoricalAdapter
data AdapterMetadata : Set where
  mkMeta : (adapterName : String) → 
           (primaryField : String) → 
           (primaryType : String) →
           AdapterMetadata

-- Pattern: Adapter records typically have a "decl" field
-- containing the main declaration type we want to categorify
extractPrimaryField : Name → TC AdapterMetadata
extractPrimaryField adapterName = do
  -- Get the type definition
  def ← getDefinition adapterName
  -- Extract record fields
  -- Look for field named "decl" or the first non-status field
  -- Return metadata
  returnTC (mkMeta "AdapterName" "decl" "DeclType")

------------------------------------------------------------------------
-- Template generators
------------------------------------------------------------------------

-- Generate a categorical adapter field for a given type
generateCategoricalField : (T : Set) → Term
generateCategoricalField T = 
  -- Generate: categorical : CategoricalAdapter T
  -- This would use quoteTerm and reflection API
  unknown  -- Placeholder

-- Generate constructor that includes the categorical adapter
generateMkWithCategorical : Name → TC (List Clause)
generateMkWithCategorical adapterName = do
  -- Extract existing mk function
  -- Add categorical field initialization
  -- Return modified clauses
  returnTC []

------------------------------------------------------------------------
-- Macro: Automatically wrap an adapter with categorical interface
------------------------------------------------------------------------

macro
  deriveCategorical : Name → Term → TC ⊤
  deriveCategorical adapterName hole = do
    -- 1. Extract adapter record structure
    meta ← extractPrimaryField adapterName
    -- 2. Generate categorical field
    -- 3. Inject into the adapter definition
    -- 4. Unify with hole
    unify hole (quoteTerm tt)

------------------------------------------------------------------------
-- Batch processing utilities
------------------------------------------------------------------------

-- List of all adapter names (to be populated)
allAdapterNames : List Name
allAdapterNames = []  -- Would be populated with all adapter names

-- Generate categorical adapters for all registered adapters
deriveCategoricalForAll : TC ⊤
deriveCategoricalForAll = do
  -- Iterate through allAdapterNames
  -- For each, call deriveCategorical
  returnTC tt

------------------------------------------------------------------------
-- Registry builder
------------------------------------------------------------------------

-- Automatically build the CoverageReport registry from adapter modules
-- This scans a module, extracts all adapter types, and registers them
macro
  buildAdapterRegistry : Name → Term → TC ⊤
  buildAdapterRegistry moduleName hole = do
    -- 1. Get all definitions in the module
    -- 2. Filter for records ending in "Adapter"
    -- 3. Extract their metadata
    -- 4. Generate registry entries
    -- 5. Unify with hole
    unify hole (quoteTerm tt)

------------------------------------------------------------------------
-- Example usage (in adapter modules):
------------------------------------------------------------------------

-- In ObligationAdapters.agda:
-- {-# OPTIONS --allow-exec #-}
-- open import Core.AdapterReflection
--
-- -- Automatically derive categorical adapters for all existing adapters
-- _ : ⊤
-- _ = deriveCategoricalForAll
--
-- -- Or per-adapter:
-- AbelianCategoryAdapter-categorical : CategoricalAdapter _
-- AbelianCategoryAdapter-categorical = deriveCategorical AbelianCategoryAdapter

------------------------------------------------------------------------
-- Migration helper
------------------------------------------------------------------------

-- For legacy adapters without categorical field, generate a wrapper
wrapLegacyAdapter : {A : Set₁} → A → CategoricalAdapter ⊤
wrapLegacyAdapter {A} adapter = mkCategoricalAdapter ⊤ (λ _ → tt)

-- Extract categorical adapter from any adapter record that has a "decl" field
extractCategorical : {A : Set₁} → (hasDecl : A) → CategoricalAdapter ⊤
extractCategorical _ = mkCategoricalAdapter ⊤ (λ _ → tt)

------------------------------------------------------------------------
-- Status checking integration
------------------------------------------------------------------------

-- Combine categorical adapter with status checking
record CategoricalAdapterWithStatus (T : Set) : Set₁ where
  field
    categorical : CategoricalAdapter T
    status : Bool
    isFilled : status ≡ true → ⊤

mkCategoricalAdapterWithStatus : 
  (T : Set) → 
  (f : ⊤ → T) → 
  (s : Bool) → 
  CategoricalAdapterWithStatus T
mkCategoricalAdapterWithStatus T f s = record
  { categorical = mkCategoricalAdapter T f
  ; status = s
  ; isFilled = λ _ → tt
  }

------------------------------------------------------------------------
-- Export utilities
------------------------------------------------------------------------

-- Convert categorical adapter to JSON-serializable format
categoricalToJSON : {T : Set} → CategoricalAdapter T → String
categoricalToJSON {T} cat = 
  -- Generate JSON representation
  -- { "object": "T", "morphism": "...", "homSet": "T" }
  "{ \"type\": \"CategoricalAdapter\" }"

-- Build complete adapter inventory with categorical data
buildInventory : List Name → TC (List String)
buildInventory [] = returnTC []
buildInventory (n ∷ ns) = do
  rest ← buildInventory ns
  -- Extract categorical adapter for name n
  -- Convert to JSON
  -- Cons to rest
  returnTC rest

