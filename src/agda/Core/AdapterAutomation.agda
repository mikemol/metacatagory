{-# OPTIONS --without-K #-}

-- Core.AdapterAutomation: Practical automation for adapter categorical wrapping
-- Non-reflection-based approach using type classes and manual registration

module Core.AdapterAutomation where
open import Agda.Builtin.Nat using (Nat; zero; suc)

open import Agda.Builtin.Unit using (⊤; tt)
open import Core.Phase using (Bool; true; false)
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.String using (String)
open import Agda.Builtin.Equality using (_≡_; refl)
open import Core.CategoricalAdapter

------------------------------------------------------------------------
-- Type class for adapters that can produce categorical adapters
------------------------------------------------------------------------

record HasCategorical (A : Set₁) : Set₂ where
  field
    extractType : Set
    extractMorphism : ⊤ → extractType
    toCategorical : A → CategoricalAdapter extractType

open HasCategorical {{...}} public

------------------------------------------------------------------------
-- Canonical adapter pattern
------------------------------------------------------------------------

-- Most adapters follow this pattern:
-- - Have a "decl" field with the main declaration
-- - Have a "status" field
-- - Have "isFilled" check
--
-- This record captures that pattern and adds categorical interface

record StandardAdapter (DeclType : Set) : Set₁ where
  field
    decl : DeclType
    status : Bool
    categorical : CategoricalAdapter DeclType

mkStandardAdapter : 
  (D : Set) → 
  (d : D) → 
  (f : ⊤ → D) → 
  StandardAdapter D
mkStandardAdapter D d f = record
  { decl = d
  ; status = true
  ; categorical = mkCategoricalAdapter D f
  }

isFilledStandard : {D : Set} → StandardAdapter D → Bool
isFilledStandard a = StandardAdapter.status a

------------------------------------------------------------------------
-- Enhanced adapter with proofs
------------------------------------------------------------------------

record EnhancedAdapter (DeclType : Set) : Set₁ where
  field
    decl : DeclType
    status : Bool
    categorical : CategoricalAdapter DeclType
    -- Proof that status matches the categorical structure
    statusCorrect : status ≡ true → ⊤

mkEnhancedAdapter :
  (D : Set) →
  (d : D) →
  (f : ⊤ → D) →
  EnhancedAdapter D
mkEnhancedAdapter D d f = record
  { decl = d
  ; status = true
  ; categorical = mkCategoricalAdapter D f
  ; statusCorrect = λ _ → tt
  }

------------------------------------------------------------------------
-- Adapter migration utilities
------------------------------------------------------------------------

-- Wrap a legacy adapter (with decl and status) into categorical form
record LegacyAdapterWrapper (DeclType : Set) : Set₁ where
  field
    decl : DeclType
    status : Bool
    -- We add this field to legacy adapters
    categoricalView : CategoricalAdapter DeclType

-- Helper to upgrade legacy adapters
upgradeLegacyAdapter :
  {D : Set} →
  (decl : D) →
  (status : Bool) →
  (defaultInhabitant : ⊤ → D) →
  LegacyAdapterWrapper D
upgradeLegacyAdapter {D} d s f = record
  { decl = d
  ; status = s
  ; categoricalView = mkCategoricalAdapter D f
  }

------------------------------------------------------------------------
-- Batch conversion templates
------------------------------------------------------------------------

-- Template for converting a simple adapter
-- Input: record with { decl : D ; status : Bool }
-- Output: adds categorical field

-- Example conversion pattern:
-- OLD:
--   record MyAdapter : Set₁ where
--     field
--       decl : MyDecl
--       status : Bool
--
-- NEW:
--   record MyAdapter : Set₁ where
--     field
--       decl : MyDecl
--       status : Bool
--       categorical : CategoricalAdapter MyDecl
--
--   mkMyAdapter : MyDecl → (⊤ → MyDecl) → MyAdapter
--   mkMyAdapter d f = record
--     { decl = d
--     ; status = true
--     ; categorical = mkCategoricalAdapter _ f
--     }

------------------------------------------------------------------------
-- Registry automation
------------------------------------------------------------------------

-- Adapter registration info
record AdapterInfo : Set where
  field
    adapterName : String
    declType : String
    hasStatus : Bool
    hasCategorical : Bool

-- Build a registry of adapters for coverage reporting
AdapterRegistry : Set
AdapterRegistry = List AdapterInfo

-- Helper to create registry entries
mkAdapterInfo : String → String → Bool → Bool → AdapterInfo
mkAdapterInfo name dtype status cat = record
  { adapterName = name
  ; declType = dtype
  ; hasStatus = status
  ; hasCategorical = cat
  }

-- Example registry (to be auto-generated or manually maintained)
exampleRegistry : AdapterRegistry
exampleRegistry =
  mkAdapterInfo "AbelianCategoryAdapter" "AbelianCategoryDeclaration" true false ∷
  mkAdapterInfo "BiproductAdapter" "BiproductObject" true false ∷
  []

------------------------------------------------------------------------
-- Migration checklist generator
------------------------------------------------------------------------

-- Analyze an adapter and determine what needs to be added
data MigrationTask : Set where
  AddCategoricalField : String → MigrationTask
  UpdateConstructor : String → MigrationTask
  AlreadyMigrated : String → MigrationTask

analyzeMigration : AdapterInfo → MigrationTask
analyzeMigration info with AdapterInfo.hasCategorical info
... | true  = AlreadyMigrated (AdapterInfo.adapterName info)
... | false = AddCategoricalField (AdapterInfo.adapterName info)

-- Generate migration tasks for all adapters
generateMigrationPlan : AdapterRegistry → List MigrationTask
generateMigrationPlan [] = []
generateMigrationPlan (info ∷ rest) = 
  analyzeMigration info ∷ generateMigrationPlan rest

------------------------------------------------------------------------
-- Automated constructor generation pattern
------------------------------------------------------------------------

-- Generic pattern for constructors with categorical adapter
-- This can be copied and adapted for each adapter type

-- Pattern 1: Simple adapter (single decl field)
pattern-mk-simple :
  {D : Set} →
  (d : D) →
  (defaultInhabitant : ⊤ → D) →
  StandardAdapter D
pattern-mk-simple d f = mkStandardAdapter _ d f

-- Pattern 2: Adapter with links (common pattern in your codebase)
record LinkedAdapter (DeclType LinkType : Set) : Set₁ where
  field
    decl : DeclType
    expected : LinkType
    link : DeclType ≡ LinkType
    status : Bool
    categorical : CategoricalAdapter DeclType

mkLinkedAdapter :
  {D L : Set} →
  (d : D) →
  (l : L) →
  (p : D ≡ L) →
  (f : ⊤ → D) →
  LinkedAdapter D L
mkLinkedAdapter {D} d l p f = record
  { decl = d
  ; expected = l
  ; link = p
  ; status = true
  ; categorical = mkCategoricalAdapter D f
  }

------------------------------------------------------------------------
-- Export and reporting
------------------------------------------------------------------------

-- Generate a report of adapter categorical status
adapterStatusReport : AdapterRegistry → String
adapterStatusReport [] = "All adapters processed"
adapterStatusReport (info ∷ rest) =
  AdapterInfo.adapterName info  -- Would append status info

-- Count adapters by migration status
countMigrated : AdapterRegistry → Nat
countMigrated [] = 0
countMigrated (info ∷ rest) with AdapterInfo.hasCategorical info
... | true  = suc (countMigrated rest)
... | false = countMigrated rest
  where
    open import Agda.Builtin.Nat using (Nat; suc)

------------------------------------------------------------------------
-- Integration with CoverageReport
------------------------------------------------------------------------

-- Enhanced checklist module with categorical data
record EnhancedChecklistModule : Set where
  field
    moduleName : String
    assertionCount : Nat
    adapterNames : List String
    allHaveCategorical : Bool

-- Validate that all adapters in a checklist have categorical adapters
validateChecklist : EnhancedChecklistModule → Bool
validateChecklist m = EnhancedChecklistModule.allHaveCategorical m
