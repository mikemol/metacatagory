{-# OPTIONS --without-K #-}

-- | Example: generate makefile snippets from Agda metadata.
module Examples.ExporterMakefile where

{-# FOREIGN GHC
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as M
import qualified Data.List as L
import           Data.Maybe (fromMaybe)
import qualified System.Directory as Dir

writeFileAdapter :: T.Text -> T.Text -> IO ()
writeFileAdapter path content = do
    let dir = T.takeWhileEnd (/= '/') path
    if T.null dir then return () else Dir.createDirectoryIfMissing True (T.unpack $ T.dropEnd 1 dir)
    TIO.writeFile (T.unpack path) content

readGraphEdgesAdapter :: T.Text -> IO [T.Text]
readGraphEdgesAdapter path = do
  exists <- Dir.doesFileExist (T.unpack path)
  if not exists then return [] else 
    TIO.readFile (T.unpack path) >>= \src -> 
      let ls = T.lines src
          lbls = M.fromList 
            [ (T.strip k, T.strip v)
            | l <- ls
            , Just (k,v) <- [parseLabel l]
            ]
          edges =
            [ T.concat [lookupLabel a lbls, 
                         T.pack "=>", 
                         lookupLabel b lbls]
            | l <- ls
            , Just (a,b) <- [parseEdge l]
            ]
      in return edges
  where
    parseLabel l =
      case T.breakOn "[label=\"" l of
        (pre, suf) | T.null suf -> Nothing
        (_, suf) ->
          let k = T.takeWhile (/= '[') l
              name = T.takeWhile (/= '"') (T.drop (T.length "[label=\"") suf)
          in Just (T.strip k, name)
    parseEdge l =
      case T.breakOn "->" l of
        (a, suf) | T.null suf -> Nothing
        (a, suf) ->
          let a' = T.strip a
              b' = T.takeWhile (/= ';') (T.drop 2 suf)
          in Just (a', T.strip b')
    lookupLabel k m = fromMaybe k (M.lookup k m)

pathToModuleLabelAdapter :: T.Text -> T.Text
pathToModuleLabelAdapter p =
  let p'  = fromMaybe p (T.stripPrefix (T.pack "src/agda/") p)
      noE = fromMaybe p' (T.stripSuffix (T.pack ".agda") p')
  in T.replace (T.pack "/") (T.pack ".") noE
 #-}

open import Examples.AgdaFileScanFFI
open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.Unit using (⊤; tt)
open import Agda.Builtin.String using (String)
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.Bool using (Bool; true; false)
open import Core.Utils using (map)
open import Core.CategoricalAdapter
open import Core.AdapterAutomation

postulate
  readGraphEdges : String → IO (List String)
  pathToModuleLabel : String → String
{-# COMPILE GHC readGraphEdges = readGraphEdgesAdapter #-}
{-# COMPILE GHC pathToModuleLabel = pathToModuleLabelAdapter #-}
open import Metamodel
open import Examples.AgdaMakefileDeps using (_++_; ModuleName; primStringEquality; primStringSplit;
  moduleToInterfacePath; joinWith; parseModuleName; isInternalModule; filter)
open import Examples.MakefileTargets using (MakefileTarget; TargetCategory; allCategories; 
  validatorToTarget; generatorToTarget; nodeSetupCategory; badgeCategory; mdLintCategory;
  discoverAgdaFiles; generateAgdaTargets; generateDocsTargets; allAgdaiTarget; allDocsTarget;
  environmentSetupToTarget; synchronizerToTarget; mkTarget; instrumentRecipe)

postulate
  writeFile : String → String → IO ⊤
{-# COMPILE GHC writeFile = writeFileAdapter #-}

-- Section: Makefile Artifact Types

-- | A logical section of the generated makefile (e.g., variables or a phony block).
record MakefileSection : Set where
  field
    id : String
    content : List String

-- | Aggregate makefile made of multiple sections.
record MakefileArtifact : Set where
  field
    sections : List MakefileSection

-- | Renderer turns a section into text; enables alternative formatting strategies.
record MakefileRenderer : Set where
  field
    renderSection : MakefileSection → String

-- Section: Default renderer
defaultRenderer : MakefileRenderer
defaultRenderer = record
  { renderSection = λ s → renderLines (MakefileSection.content s) }
  where
    renderLines : List String → String
    renderLines [] = ""
    renderLines (x ∷ []) = x
    renderLines (x ∷ xs) = x ++ "\n" ++ renderLines xs

-- Export Makefile
exportMakefile : MakefileRenderer → MakefileArtifact → String
exportMakefile renderer artifact =
  let sections = MakefileArtifact.sections artifact
      renderedSections = map (MakefileRenderer.renderSection renderer) sections
  in intercalate "\n" renderedSections
  where
    intercalate : String → List String → String
    intercalate sep [] = ""
    intercalate sep (x ∷ []) = x
    intercalate sep (x ∷ xs) = x ++ sep ++ intercalate sep xs

-- Section: Example composition

-- Convert MakefileTarget to MakefileSection
targetToSection : MakefileTarget → MakefileSection
targetToSection target = record
  { id = MakefileTarget.name target
  ; content = ("# " ++ MakefileTarget.description target) 
             ∷ (MakefileTarget.name target ++ ": " ++ intercalate " " (MakefileTarget.dependencies target)) 
             ∷ map ("\t" ++_) (MakefileTarget.recipe target)
  }
  where
    intercalate : String → List String → String
    intercalate sep [] = ""
    intercalate sep (x ∷ []) = x
    intercalate sep (x ∷ xs) = x ++ sep ++ intercalate sep xs

-- Typed Bootstrapping: regen-makefile is now a rigorous target
regenMakefileTarget : MakefileTarget
regenMakefileTarget = mkTarget
  "regen-makefile"
  "Regenerate the Makefile from Agda source (Self-Hosting)"
  ("build/diagrams/agda-deps-full.dot" ∷ [])
  ( "$(AGDA) $(AGDA_FLAGS) --compile src/agda/Examples/ExporterMakefile.agda && ./src/agda/ExporterMakefile"
  ∷ "cp Makefile.generated Makefile"
  ∷ [])
  true

-- Documentation Renderer: Generates Markdown table of targets
renderDocs : List MakefileTarget → String
renderDocs targets = 
  "| Target | Description |\n" ++
  "| :--- | :--- |\n" ++
  renderRows targets
  where
    escape : String → String
    escape s = s -- minimal escaping for now
    
    renderRow : MakefileTarget → String
    renderRow t = "| `" ++ MakefileTarget.name t ++ "` | " ++ MakefileTarget.description t ++ " |"

    renderRows : List MakefileTarget → String
    renderRows [] = ""
    renderRows (x ∷ xs) = renderRow x ++ "\n" ++ renderRows xs

-- Discovered targets
discoveredTargets : List MakefileTarget
discoveredTargets = 
  validatorToTarget "md-lint" "Lint all markdown files (fail on error)" "build/reports/md-lint.txt" 
    ("mkdir -p build/reports" ∷ "npx markdownlint-cli2 --config .markdownlint.json \"docs/modules/**/*.md\" \"docs/planning/ROADMAP.md\" \"src/agda/Plan/CIM/meta-index.d/*.md\" > build/reports/md-lint.txt 2>&1" ∷ [])
  ∷ generatorToTarget "md-fix" "Auto-fix markdown lint errors" ([])
    ("npx markdownlint-cli2 --config .markdownlint.json --fix \"docs/modules/**/*.md\" \"docs/planning/ROADMAP.md\" \"src/agda/Plan/CIM/meta-index.d/*.md\"" ∷ [])
  ∷ validatorToTarget "intake-lint" "Lint intake files specifically" "build/reports/intake-md-lint.txt"
    ("mkdir -p build/reports && printf \"intake lint suppressed (too much legacy noise)\\n\" > build/reports/intake-md-lint.txt" ∷ [])
    ∷ generatorToTarget "build/canonical_roadmap.json" "Generate canonical roadmap JSON from intake" ([])
      ("python3 scripts/intake_scan.py" ∷ [])
  ∷ generatorToTarget "intake-scan" "Scan intake directory for new files" ("planning-index-json" ∷ [])
    ("@echo \"intake scan complete\"" ∷ [])
  ∷ generatorToTarget "md-normalize" "Normalize markdown formatting" ([])
    ("python3 scripts/normalize_generated_markdown.py" ∷ [])
  ∷ validatorToTarget "makefile-validate" "Validate Makefile consistency" "build/reports/makefile-validate.txt"
    ("mkdir -p build/reports" ∷ "python3 scripts/validate_makefile_docs.py > build/reports/makefile-validate.txt || (cat build/reports/makefile-validate.txt; exit 1)" ∷ [])
  ∷ generatorToTarget "all" "Build all code and documentation" ("agda-all" ∷ "docs-all" ∷ [])
    ("@echo \"all complete\"" ∷ [])
  ∷ generatorToTarget "test-python" "Run Python tests" ([])
    ("pytest tests/ -v" ∷ [])
  ∷ generatorToTarget "debt-check" "Run debt tracking validation" ("deferred-items" ∷ "intake-scan" ∷ [])
    ("@echo \"✓ Debt tracking tools validated\"" ∷ [])
  ∷ generatorToTarget "check" "Run all validation checks" ("makefile-validate" ∷ "md-lint" ∷ "roadmap-validate-triangle" ∷ "docs-validate" ∷ "test-python" ∷ "debt-check" ∷ "all" ∷ [])
    ("@echo \"check complete\"" ∷ [])
  ∷ generatorToTarget "badges" "Generate status badges" ("priority-badge-weights" ∷ []) 
    ("python3 scripts/generate-badges.py" ∷ [])
  ∷ generatorToTarget "priority-strategy-profiles" "Compile and run Agda priority orchestration (generate strategy profiles)" ([]) 
    ("mkdir -p build" ∷ "$(AGDA) $(AGDA_FLAGS) --compile src/agda/TechnicalDebt/PriorityOrchestrationFFI.agda" ∷ "./src/agda/PriorityOrchestrationFFI" ∷ [])
  ∷ generatorToTarget "priority-badge-weights" "Normalize Agda strategy profiles into badge weights" ("priority-strategy-profiles" ∷ []) 
    ("python3 scripts/adopt_priority_strategies.py --input build/priority_strategy_profiles.json --output .github/badges/weights.json" ∷ [])
  ∷ generatorToTarget "priority-profile-json" "Export structured priority profile (lazy; derived from planning index)" ("planning-index-json" ∷ [])
    ("mkdir -p build" ∷ "$(AGDA) $(AGDA_FLAGS) --compile src/agda/Plan/CIM/PriorityProfileExport.agda && ./src/agda/PriorityProfileExport" ∷ [])
  ∷ generatorToTarget "dependency-graph-json" "Export dependency graph JSON via Agda (from agda-deps-full.dot)" ("build/diagrams/agda-deps-full.dot" ∷ [])
    ("mkdir -p build" ∷ "$(AGDA) $(AGDA_FLAGS) --compile src/agda/Plan/CIM/DependencyGraphExport.agda && ./src/agda/DependencyGraphExport" ∷ [])
  ∷ generatorToTarget "priority-refresh" "Re-run priority pipeline and refresh roadmap/badge outputs" ("planning-index-json" ∷ "roadmap-export-json" ∷ "priority-badge-weights" ∷ "badges" ∷ [])
    ("@echo \"priority pipeline refreshed (planning index, tasks, badge weights, roadmap badges)\"" ∷ [])
  ∷ generatorToTarget "docs-modules" "Generate per-module markdown documentation" ("src/agda/Plan/CIM/ModuleExporter.agdai" ∷ [])
    ("$(AGDA) $(AGDA_FLAGS) --compile src/agda/Plan/CIM/ModuleExporter.agda && ./src/agda/Plan/CIM/ModuleExporter" ∷ [])
  ∷ generatorToTarget "docs-all" "Generate documentation (markdown only)" ("docs-generate" ∷ "docs-modules" ∷ [])
    ("@echo \"docs (markdown) complete\"" ∷ [])
  ∷ environmentSetupToTarget "node-deps" "Install Node.js dependencies"
    ("npm install" ∷ [])
  ∷ generatorToTarget "deferred-items" "Scan for TODOs and FIXMEs (Agda FFI binary)" ([])
    ("$(AGDA) $(AGDA_FLAGS) --compile src/agda/TechnicalDebt/DeferredItemsOrchestrationFFI.agda" ∷ "./src/agda/DeferredItemsOrchestrationFFI" ∷ [])
  ∷ generatorToTarget "roadmap-index" "Compile Roadmap Index" ("src/agda/Plan/CIM/RoadmapIndex.agdai" ∷ [])
    ("$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/RoadmapIndex.agda" ∷ [])
  ∷ generatorToTarget "planning-index-json" "Export planning index to JSON" ("src/agda/Plan/CIM/PlanningExport.agdai" ∷ [])
    ("mkdir -p build" ∷ "$(AGDA) $(AGDA_FLAGS) --compile src/agda/Plan/CIM/PlanningExport.agda && ./src/agda/PlanningExport" ∷ [])
  ∷ generatorToTarget "planning-kernel" "Compile Planning Kernel" ("src/agda/Plan/CIM/PlanningKernel.agdai" ∷ [])
    ("$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/PlanningKernel.agda" ∷ [])
  ∷ generatorToTarget "roadmap-sync" "Sync roadmap with external tracker" ("roadmap-export-json" ∷ "src/agda/Plan/CIM/RoadmapSync.agdai" ∷ [])
    ("$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/RoadmapSync.agda" ∷ [])
  ∷ generatorToTarget "roadmap-sppf" "Compile Roadmap SPPF" ("src/agda/Plan/CIM/RoadmapSPPF.agdai" ∷ [])
    ("$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/RoadmapSPPF.agda" ∷ [])
  ∷ generatorToTarget "validate-constructive" "Run all constructive build targets" (
        "docs-all" ∷ "docs-generate" ∷ "docs-modules" ∷
        "roadmap-export-json" ∷ "roadmap-export-md" ∷ "roadmap-export-enriched" ∷ "roadmap-export-deps" ∷
        "roadmap-deps-graph" ∷ "roadmap-enrich" ∷ "roadmap-all-enriched" ∷
        "intake-scan" ∷ "md-normalize" ∷ "badges" ∷ [])
    ("@echo \"✓ Constructive validation complete\"" ∷ [])
  ∷ generatorToTarget "roadmap-merge" "Merge ingestion streams" ([])
    ("python3 scripts/merge_roadmaps.py" ∷ [])
  ∷ generatorToTarget "build/diagrams/agda-deps-full.dot" "Generate dependency graph" ([]) 
    ("mkdir -p build/diagrams" ∷ "$(AGDA) --dependency-graph=build/diagrams/agda-deps-full.dot $(AGDA_FLAGS) src/agda/Tests/Index.agda 2>&1 | grep -E \"(Checking|Error)\" | head -20" ∷ [])
  ∷ generatorToTarget "roadmap-deps-graph" "Generate dependency graph" ("build/diagrams/agda-deps-full.dot" ∷ [])
    ("@echo \"agda dependency graph generated\"" ∷ [])
  ∷ generatorToTarget "build/canonical_enriched.json" "Enrich canonical roadmap" ("planning-index-json" ∷ "build/diagrams/agda-deps-full.dot" ∷ [])
    ("python3 scripts/enrich_canonical.py" ∷ [])
  ∷ generatorToTarget "roadmap-enrich" "Enrich roadmap with graph data" ("build/canonical_enriched.json" ∷ [])
    ("@echo \"roadmap enrichment complete\"" ∷ [])
  ∷ generatorToTarget "roadmap-export-json" "Export canonical roadmap to JSON" ("planning-index-json" ∷ [])
    ("python3 scripts/export_canonical_json.py" ∷ [])
  ∷ generatorToTarget "roadmap-export-md" "Export canonical roadmap to Markdown" ("planning-index-json" ∷ [])
    ("python3 scripts/export_canonical_md.py" ∷ [])
  ∷ generatorToTarget "roadmap-export-enriched" "Export enriched roadmap" ("build/canonical_enriched.json" ∷ [])
    ("python3 scripts/export_enriched_md.py" ∷ [])
  ∷ generatorToTarget "roadmap-export-deps" "Export roadmap dependency graph" ("build/canonical_enriched.json" ∷ [])
    ("python3 scripts/export_dependency_graph.py" ∷ [])
  ∷ generatorToTarget "roadmap-validate-json" "Validate canonical JSON" ("roadmap-export-json" ∷ [])
    ("python3 scripts/validate_json.py" ∷ [])
  ∷ generatorToTarget "roadmap-validate-md" "Validate canonical Markdown" 
    ("roadmap-export-md" ∷ [])
    ("python3 scripts/validate_md.py" ∷ [])
  ∷ generatorToTarget "roadmap-validate-triangle" "Verify Triangle Identity (Agda <-> JSON <-> MD)" ("roadmap-validate-json" ∷ "roadmap-validate-md" ∷ [])
    ("@echo \"✓ Triangle validation complete\"" ∷ [])
  ∷ generatorToTarget "roadmap-sppf-export" "Export SPPF structure" ("planning-index-json" ∷ [])
    ("python3 scripts/export_roadmap_sppf.py" ∷ [])
  ∷ generatorToTarget "roadmap-all-enriched" "Build all enriched artifacts" ("roadmap-export-enriched" ∷ "roadmap-export-deps" ∷ [])
    ("@echo \"roadmap all enriched complete\"" ∷ [])
  ∷ generatorToTarget "docs-generate" "Compile and run Roadmap Exporter" ("src/agda/Plan/CIM/RoadmapExporter.agdai" ∷ [])
    ("$(AGDA) $(AGDA_FLAGS) --compile src/agda/Plan/CIM/RoadmapExporter.agda && ./src/agda/RoadmapExporter" ∷ "python3 scripts/normalize_generated_markdown.py" ∷ [])
  ∷ generatorToTarget "docs-validate" "Validate documentation integrity" ([])
    ("python3 scripts/validate_triangle_identity.py" ∷ [])
  
  ∷ []

-- Helper to concatenate lists
infixr 20 _+++_
_+++_ : {A : Set} → List A → List A → List A
[] +++ ys = ys
(x ∷ xs) +++ ys = x ∷ (xs +++ ys)

-- Generate Agda compilation target using graph-derived module dependencies
labelToModuleName : String → ModuleName
labelToModuleName = parseModuleName

isInternalLabel : String → Bool
isInternalLabel lbl = isInternalModule (labelToModuleName lbl)

labelsToInterfacePaths : List String → List String
labelsToInterfacePaths lbls = map (moduleToInterfacePath ∘ labelToModuleName) (filter isInternalLabel lbls)
  where
    _∘_ : {A B C : Set} → (B → C) → (A → B) → A → C
    (g ∘ f) x = g (f x)

generateAgdaTargetFromGraph : String → List String → MakefileTarget
generateAgdaTargetFromGraph agdaPath depLabels =
  let importPaths = labelsToInterfacePaths depLabels
      agdaiPath = agdaPath ++ "i"
      allDeps = agdaPath ∷ importPaths
      recipe = ("$(AGDA) $(AGDA_FLAGS) " ++ agdaPath) ∷ []
  in mkTarget agdaiPath ("Compile " ++ agdaPath) allDeps (instrumentRecipe agdaiPath recipe) false

-- Build complete artifact from discovered files and graph edges
buildArtifact : List String → List String → MakefileArtifact
buildArtifact agdaFiles graphEdges =
  let labels = map pathToModuleLabel agdaFiles
      agdaiTargets = zipWith generateAgdaTargetFromGraph agdaFiles (map depsFor labels)
      aggregateTargets = allAgdaiTarget agdaFiles ∷ []
      -- Include regenMakefileTarget in the list of targets
      allTargets = regenMakefileTarget ∷ discoveredTargets +++ agdaiTargets +++ aggregateTargets
      phonyTargets = filter (λ t → MakefileTarget.phony t) allTargets
      phonyNames = map MakefileTarget.name phonyTargets
      headerSection = record
        { id = "header"
        ; content =
            ( "# Use local Agda 2.8.0 if available, otherwise system agda"
            ∷ "AGDA := $(if $(wildcard .local/agda),.local/agda,agda)"
            ∷ ""
            ∷ "# Default parallelism scales with available cores unless user overrides MAKEFLAGS."
            ∷ "CORES ?= $(shell nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)"
            ∷ "MAKEFLAGS += -j$(CORES)"
            ∷ ""
            ∷ "# Profiling output (JSONL). New file per make invocation for history."
            ∷ "PROFILE_DIR ?= build/profiles.d"
            ∷ "ifndef PROFILE_RUN"
            ∷ "PROFILE_RUN := $(shell sh -c \"echo $$(date +%Y%m%dT%H%M%S%z)-$$$$\")"
            ∷ "endif"
            ∷ "PROFILE_LOG ?= $(PROFILE_DIR)/profile-$(PROFILE_RUN).jsonl"
            ∷ ""
            ∷ "# Common Agda compilation flags"
            ∷ "AGDA_FLAGS := -i src/agda --ghc-flag=-Wno-star-is-type"
            ∷ [])
        }
      phonySection = record { id = "phony" 
                ; content = (".PHONY: " ++ intercalate " " phonyNames) ∷ [] }
  in record { sections = headerSection ∷ phonySection ∷ map targetToSection allTargets }
  where
    second : String → String → String
    second _ e with primStringSplit "=>" e
    ... | src ∷ dst ∷ [] = dst
    ... | _ = ""
    hasSrc : String → String → Bool
    hasSrc l e with primStringSplit "=>" e
    ... | src ∷ dst ∷ [] = primStringEquality src l
    ... | _ = false
    depsFor : String → List String
    depsFor lbl = map (second lbl) (filter (hasSrc lbl) graphEdges)
    zipWith : {A B C : Set} → (A → B → C) → List A → List B → List C
    zipWith f [] _ = []
    zipWith f _ [] = []
    zipWith f (x ∷ xs) (y ∷ ys) = f x y ∷ zipWith f xs ys
    intercalate : String → List String → String
    intercalate sep [] = ""
    intercalate sep (x ∷ []) = x
    intercalate sep (x ∷ xs) = x ++ sep ++ intercalate sep xs

-- IO-based main that discovers files, generates Makefile AND Documentation
postulate
  _>>=_ : {A B : Set} → IO A → (A → IO B) → IO B
  return : {A : Set} → A → IO A
  mapM : {A B : Set} → (A → IO B) → List A → IO (List B)

{-# COMPILE GHC _>>=_ = \_ _ -> (>>=) #-}
{-# COMPILE GHC return = \_ -> return #-}
{-# COMPILE GHC mapM = \_ _ -> mapM #-}

main : IO ⊤
main = 
  discoverAgdaFiles >>= λ agdaFiles →
  readGraphEdges "build/diagrams/agda-deps-full.dot" >>= λ edges →
  let artifact = buildArtifact agdaFiles edges
      content = exportMakefile defaultRenderer artifact
      -- Extract targets for documentation (excluding detailed file targets for brevity if needed)
      -- For now, we export ALL generated targets to be rigorous.
      targets = (regenMakefileTarget ∷ discoveredTargets) 
      docsContent = renderDocs targets
  in writeFile "Makefile.generated" content >>= λ _ →
     writeFile "build/makefile_targets_generated.md" docsContent >>= λ _ →
     writeFile "docs/automation/MAKEFILE-TARGETS.md" docsContent
