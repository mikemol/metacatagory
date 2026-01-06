{-# OPTIONS --without-K #-}
module Examples.Makefile.Targets.Docs where

open import Agda.Builtin.List using (List; _∷_; [])
open import Examples.MakefileTargets using (MakefileTarget; generatorToTarget; validatorToTarget)

-- Documentation and markdown lint/fix/validate targets

-- Lint/fix and doc generation/validation
docTargets : List MakefileTarget
docTargets =
  validatorToTarget "md-lint" "Lint all markdown files (fail on error)" "build/reports/md-lint.txt" 
    ("mkdir -p build/reports" ∷ "npx markdownlint-cli2 --config .markdownlint.json \"docs/modules/**/*.md\" \"docs/planning/ROADMAP.md\" \"src/agda/Plan/CIM/meta-index.d/*.md\" > build/reports/md-lint.txt 2>&1" ∷ [])
  ∷ generatorToTarget "md-fix" "Auto-fix markdown lint errors" ([])
    ("npx markdownlint-cli2 --config .markdownlint.json --fix \"docs/modules/**/*.md\" \"docs/planning/ROADMAP.md\" \"src/agda/Plan/CIM/meta-index.d/*.md\"" ∷ [])
  ∷ generatorToTarget "md-normalize" "Normalize markdown formatting" ([]) 
    ("python3 scripts/normalize_generated_markdown.py" ∷ [])
  ∷ generatorToTarget "docs-generate" "Compile and run Roadmap Exporter" ("src/agda/Plan/CIM/RoadmapExporter.agdai" ∷ [])
    ("$(AGDA) $(AGDA_FLAGS) --compile src/agda/Plan/CIM/RoadmapExporter.agda && ./src/agda/RoadmapExporter" ∷ "python3 scripts/normalize_generated_markdown.py" ∷ [])
  ∷ generatorToTarget "docs-modules" "Generate per-module markdown documentation" ("src/agda/Plan/CIM/ModuleExporter.agdai" ∷ [])
    ("$(AGDA) $(AGDA_FLAGS) --compile src/agda/Plan/CIM/ModuleExporter.agda && ./src/agda/Plan/CIM/ModuleExporter" ∷ [])
  ∷ generatorToTarget "docs-all" "Generate documentation (markdown only)" ("docs-generate" ∷ "docs-modules" ∷ [])
    ("@echo \"docs (markdown) complete\"" ∷ [])
  ∷ generatorToTarget "docs-validate" "Validate documentation integrity" ([]) 
    ("python3 scripts/validate_triangle_identity.py" ∷ [])
  ∷ []
