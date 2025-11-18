# Adapter Migration Guide

## Overview

This guide provides templates and automation tools for adding categorical adapters to all existing adapter records in the codebase.

## Core Components

1. **Core.CategoricalAdapter** - Universal categorical interface
2. **Core.AdapterAutomation** - Type classes and patterns for adapter wrapping
3. **Core.AdapterReflection** - Metaprogramming utilities (advanced)
4. **scripts/migrate_adapters.py** - Python script for automated migration

## Migration Strategy

### Phase 1: Manual Template Application (Recommended Start)

For each adapter, follow this template:

#### Before (Legacy Adapter):
```agda
record MyAdapter : Set₁ where
  field
    decl : MyDeclaration
    expected : SomeType
    link : MyDeclaration.field decl ≡ expected
    status : Bool

mkMyAdapter :
  (d : MyDeclaration) →
  (e : SomeType) →
  (p : MyDeclaration.field d ≡ e) →
  MyAdapter
mkMyAdapter d e p = record
  { decl = d
  ; expected = e
  ; link = p
  ; status = true
  }

isFilledMy : MyAdapter → Bool
isFilledMy a = MyAdapter.status a
```

#### After (With Categorical Adapter):
```agda
open import Core.CategoricalAdapter

record MyAdapter : Set₁ where
  field
    decl : MyDeclaration
    expected : SomeType
    link : MyDeclaration.field decl ≡ expected
    status : Bool
    categorical : CategoricalAdapter MyDeclaration  -- NEW

mkMyAdapter :
  (d : MyDeclaration) →
  (e : SomeType) →
  (p : MyDeclaration.field d ≡ e) →
  (f : ⊤ → MyDeclaration) →  -- NEW PARAMETER
  MyAdapter
mkMyAdapter d e p f = record
  { decl = d
  ; expected = e
  ; link = p
  ; status = true
  ; categorical = mkCategoricalAdapter MyDeclaration f  -- NEW
  }

isFilledMy : MyAdapter → Bool
isFilledMy a = MyAdapter.status a
```

### Phase 2: Batch Migration with Patterns

Use `Core.AdapterAutomation` for common patterns:

#### Simple Adapter (single decl field):
```agda
open import Core.AdapterAutomation

-- Use StandardAdapter template
myAdapter : StandardAdapter MyDeclaration
myAdapter = mkStandardAdapter MyDeclaration myDecl (λ _ → myDecl)
```

#### Linked Adapter (decl + expected + link):
```agda
-- Use LinkedAdapter template
myAdapter : LinkedAdapter MyDeclaration SomeType
myAdapter = mkLinkedAdapter myDecl expected proof (λ _ → myDecl)
```

### Phase 3: Automated Migration

Run the Python migration script:

```bash
# Generate migration report
python3 scripts/migrate_adapters.py src/agda/Tests/

# Migrate a specific file
python3 scripts/migrate_adapters.py src/agda/Tests/ObligationAdapters.agda
```

## Step-by-Step Migration Process

### Step 1: Import the Categorical Adapter Module

Add to the top of your adapter file:
```agda
open import Core.CategoricalAdapter
```

### Step 2: Add Categorical Field to Each Adapter

For each `record XxxAdapter`, add:
```agda
categorical : CategoricalAdapter <PrimaryType>
```

Where `<PrimaryType>` is typically the type of the `decl` field.

### Step 3: Update Constructors

Add a parameter for the morphism from terminal:
```agda
(f : ⊤ → <PrimaryType>)
```

Initialize the categorical field:
```agda
; categorical = mkCategoricalAdapter <PrimaryType> f
```

### Step 4: Update Call Sites

When constructing adapters in test files, provide the morphism:
```agda
-- Before:
myAdapt = mkMyAdapter myDecl expected proof

-- After:
myAdapt = mkMyAdapter myDecl expected proof (λ _ → myDecl)
```

The morphism `(λ _ → myDecl)` maps the terminal object ⊤ to your declaration.

## Migration Checklist

- [ ] Add `Core.CategoricalAdapter` import
- [ ] For each adapter record:
  - [ ] Add `categorical` field
  - [ ] Update constructor signature
  - [ ] Initialize categorical field in constructor
- [ ] Update all call sites in test files
- [ ] Verify compilation: `agda --no-main <file>`
- [ ] Update CoverageReport registry if needed

## Common Patterns

### Pattern 1: Direct Morphism (Most Common)
```agda
mkMyAdapter decl ... (λ _ → decl)
```

### Pattern 2: Extracted Field
```agda
mkMyAdapter decl field ... (λ _ → field)
```

### Pattern 3: Constructed Value
```agda
mkMyAdapter decl ... (λ _ → MyDeclaration.construct ...)
```

## Benefits After Migration

1. **Unified Interface**: All adapters expose categorical structure
2. **Automated Export**: Can generate JSON/reports from categorical data
3. **Consistency Checking**: Type system enforces correct categorical relationships
4. **Future Proofing**: Easier to add new categorical properties later

## Validation

After migration, verify:

1. All files compile without errors
2. `make check-coverage` passes
3. Test assertions still pass
4. No broken links in CoverageReport

## Troubleshooting

### Error: "Not in scope: CategoricalAdapter"
**Solution**: Add `open import Core.CategoricalAdapter` to the file.

### Error: "Missing argument in constructor"
**Solution**: Add the morphism parameter `(λ _ → decl)` to adapter construction.

### Error: "Type mismatch in categorical field"
**Solution**: Ensure the type parameter to `mkCategoricalAdapter` matches the `decl` field type.

## Next Steps

1. Start with ToposObligationAdapters (already isolated)
2. Migrate one adapter as a test case
3. Apply to all adapters in the file
4. Extend to other adapter files
5. Update coverage reporting to use categorical data

