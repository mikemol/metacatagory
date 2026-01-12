{-# OPTIONS --without-K #-}
-- | Documentation and markdown validation targets.
module Examples.Makefile.Targets.Docs where

open import Agda.Builtin.List using (List; _∷_; [])
open import Examples.MakefileTargets using (MakefileTarget; generatorToTarget; generatorToFileTarget; validatorToTarget; mutateCert)

-- Documentation and markdown lint/fix/validate targets

-- Lint/fix and doc generation/validation
docTargets : List MakefileTarget
docTargets =
  validatorToTarget "md-lint" "Lint all markdown files (fail on error)" "build/reports/md-lint.txt"
    []
    ("if [ \"$${MUTATE_OK:-}\" = \"1\" ] && [ \"$${METACATAGORY_REPORT_MODE:-stdout}\" = \"write\" ]; then mkdir -p build/reports && npx markdownlint-cli2 --config .markdownlint.json \"docs/modules/**/*.md\" \"docs/planning/ROADMAP.md\" \"src/agda/Plan/CIM/meta-index.d/*.md\" > build/reports/md-lint.txt 2>&1; else npx markdownlint-cli2 --config .markdownlint.json \"docs/modules/**/*.md\" \"docs/planning/ROADMAP.md\" \"src/agda/Plan/CIM/meta-index.d/*.md\"; fi" ∷ [])
  ∷ generatorToFileTarget mutateCert "build/reports/docs-lint.json" "Emit Agda docs lint report"
    []
    ("METACATAGORY_REPORT_MODE=write python3 scripts/lint_agda_docs.py" ∷ [])
  ∷ validatorToTarget "docs-lint" "Lint Agda docs coverage (fail on error)" "build/reports/docs-lint.json"
    []
    ("python3 scripts/lint_agda_docs.py" ∷ [])
  ∷ generatorToTarget mutateCert "md-fix" "Auto-fix markdown lint errors" ([])
    ("npx markdownlint-cli2 --config .markdownlint.json --fix \"docs/modules/**/*.md\" \"docs/planning/ROADMAP.md\" \"src/agda/Plan/CIM/meta-index.d/*.md\"" ∷ [])
  ∷ generatorToTarget mutateCert "md-normalize" "Normalize markdown formatting" ([]) 
    ("python3 scripts/normalize_generated_markdown.py" ∷ [])
  ∷ generatorToFileTarget mutateCert "build/agda/Plan/CIM/RoadmapExporterMain" "Compile roadmap exporter binary" ("src/agda/Plan/CIM/RoadmapExporterMain.agda" ∷ [])
    ("if [ -n \"$${SKIP_GHC_BACKEND}\" ]; then echo \"skip roadmap-exporter-compile (SKIP_GHC_BACKEND)\"; else $(AGDA_COMPILE) src/agda/Plan/CIM/RoadmapExporterMain.agda; fi" ∷ [])
  ∷ generatorToFileTarget mutateCert "build/reports/roadmap_ast.txt" "Export roadmap AST report"
    ("build/reports/dir.stamp" ∷ "build/agda/Plan/CIM/RoadmapExporterMain" ∷ [])
    ("if [ -n \"$${SKIP_GHC_BACKEND}\" ]; then echo \"skip build/reports/roadmap_ast.txt (SKIP_GHC_BACKEND)\"; else $(AGDA_COMPILE_DIR)/RoadmapExporterMain; fi" ∷ [])
  ∷ generatorToFileTarget mutateCert "build/agda/Plan/CIM/ModuleExporter" "Compile module exporter binary" ("src/agda/Plan/CIM/ModuleExporter.agda" ∷ [])
    ("if [ -n \"$${SKIP_GHC_BACKEND}\" ]; then echo \"skip module-exporter-compile (SKIP_GHC_BACKEND)\"; else $(AGDA_COMPILE) src/agda/Plan/CIM/ModuleExporter.agda; fi" ∷ [])
  ∷ generatorToTarget mutateCert "docs-modules" "Generate per-module markdown documentation" ("build/agda/Plan/CIM/ModuleExporter" ∷ [])
    ("if [ -n \"$${SKIP_GHC_BACKEND}\" ]; then echo \"skip docs-modules (SKIP_GHC_BACKEND)\"; else $(AGDA_COMPILE_DIR)/ModuleExporter; fi" ∷ [])
  ∷ generatorToTarget mutateCert "docs-normalize" "Normalize generated markdown after docs export"
    ("build/reports/roadmap_ast.txt" ∷ "docs-modules" ∷ [])
    ("python3 scripts/normalize_generated_markdown.py" ∷ [])
  ∷ generatorToTarget mutateCert "docs-all" "Generate documentation (markdown only)" ("docs-normalize" ∷ [])
    ("@echo \"docs (markdown) complete\"" ∷ [])
  ∷ generatorToTarget mutateCert "docs-validate" "Validate documentation integrity" ("data/planning_index.json" ∷ "ROADMAP.md" ∷ []) 
    ("python3 scripts/validate_triangle_identity.py" ∷ [])
  ∷ []
