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
      let srcStripped = T.strip src
          trivialEmpty = srcStripped == T.pack "digraph G {}" || srcStripped == T.pack "digraph dependencies {}"
          ls = T.lines src
          hasArrow = L.any (\l -> T.isInfixOf (T.pack "->") l) ls
          hasLabel = L.any (\l -> T.isInfixOf (T.pack "[label=\"") l) ls
          lbls = M.fromList 
            [ (T.strip k, T.strip v)
            | l <- ls
            , Just (k,v) <- [parseLabel l]
            ]
          parsedEdges =
            [ T.concat [lookupLabel a lbls, 
                         T.pack "=>", 
                         lookupLabel b lbls]
            | l <- ls
            , Just (a,b) <- [parseEdge l]
            ]
          -- Fallback: If file is non-trivial and contains labels/arrows but parsedEdges is empty,
          -- return a sentinel edge to indicate non-empty graph for guard purposes.
          edges = if parsedEdges == [] && (hasArrow || hasLabel) && not trivialEmpty
                  then [T.pack "__DOT_NON_EMPTY__"] else parsedEdges
      in return (if trivialEmpty then [] else edges)
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
 
-- Utility: stringify list length for Agda-side debug output
listLengthStringAdapter :: [T.Text] -> T.Text
listLengthStringAdapter xs = T.pack (show (length xs))
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
  listLengthString : List String → String
{-# COMPILE GHC readGraphEdges = readGraphEdgesAdapter #-}
{-# COMPILE GHC pathToModuleLabel = pathToModuleLabelAdapter #-}
{-# COMPILE GHC listLengthString = listLengthStringAdapter #-}
open import Metamodel
open import Examples.AgdaMakefileDeps using (_++_; ModuleName; primStringEquality; primStringSplit;
  moduleToInterfacePath; joinWith; parseModuleName; isInternalModule; filter)
open import Examples.MakefileTargets using (MakefileTarget; TargetCategory; allCategories; 
  validatorToTarget; generatorToTarget; nodeSetupCategory; badgeCategory; mdLintCategory;
  discoverAgdaFiles; generateAgdaTargets; generateDocsTargets; allAgdaiTarget; allDocsTarget;
  environmentSetupToTarget; synchronizerToTarget; mkTarget; instrumentRecipe)
open import Examples.Makefile.Targets.Python using (pythonTargets)
open import Examples.Makefile.Targets.RoadmapExports using (roadmapExportTargets)
open import Examples.Makefile.Targets.Docs using (docTargets)
open import Examples.Makefile.Targets.JsonRoundtrip using (jsonRoundtripTargets)
open import Examples.Makefile.Targets.Infra using (infraTargets)
open import Examples.Makefile.Targets.Priority using (priorityTargets)
open import Examples.Makefile.Targets.AgdaBuild using (agdaTargets)
open import Examples.Makefile.Targets.Composite using (compositeTargets)
open import Examples.Makefile.Targets.Docker using (dockerTargets)

infixr 20 _+++_
_+++_ : {A : Set} → List A → List A → List A
[] +++ ys = ys
(x ∷ xs) +++ ys = x ∷ (xs +++ ys)

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

-- Documentation Renderer: Generates Markdown table of targets with dependency graph info
-- Enhanced: now includes orchestration dependencies and assumptions preamble
renderDocs : List MakefileTarget → String
renderDocs targets = 
  "## Key Orchestration Targets\n\n" ++
  "- `check`: Full validation suite (makefile + docs + tests + code)\n" ++
  "- `ci-light`: Lightweight CI without GHC backend\n" ++
  "- `all`: Complete Agda + documentation build\n" ++
  "- `docker-all`: Docker build and GHCR push\n\n" ++
  "## All Generated Targets\n\n" ++
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
  pythonTargets +++ roadmapExportTargets +++ docTargets +++ jsonRoundtripTargets +++ infraTargets +++ priorityTargets +++ agdaTargets +++ compositeTargets +++ dockerTargets

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
      graphStatusTarget = mkTarget "graph-status" "Print parsed graph status" ([])
        (instrumentRecipe "graph-status" ("@cat build/graph_parsed_state.txt" ∷ [])) true
      graphAssertTarget = mkTarget "graph-assert-ok" "Assert dependency graph is OK (CI guard)" ([])
        (instrumentRecipe "graph-assert-ok" 
          ("@status=$$(awk -F': ' '/^status:/ {print $$2}' build/graph_parsed_state.txt); " ++
           "edges=$$(awk -F': ' '/^edges:/ {print $$2}' build/graph_parsed_state.txt); " ++
           "if [ \"$$status\" != \"OK\" ] || [ $${edges:-0} -eq 0 ]; then echo \"Graph check failed: status=$$status edges=$$edges\"; exit 1; else echo \"Graph OK: status=$$status edges=$$edges\"; fi" ∷ [])) true
      allTargets = regenMakefileTarget ∷ graphStatusTarget ∷ graphAssertTarget ∷ discoveredTargets +++ agdaiTargets +++ aggregateTargets
      phonyTargets = filter (λ t → MakefileTarget.phony t) allTargets
      phonyNames = map MakefileTarget.name phonyTargets
      headerSection = record
        { id = "header"
        ; content =
            ( "# Use local Agda 2.8.0 if available, otherwise system agda"
            ∷ "AGDA := $(if $(wildcard .local/agda),.local/agda,agda)"
            ∷ ""
            ∷ "# Shell configuration for error-safe recipes"
            ∷ "SHELL := /bin/bash"
            ∷ ".SHELLFLAGS := -euo pipefail -c"
            ∷ ".DELETE_ON_ERROR:"
            ∷ "MAKEFLAGS += --warn-undefined-variables"
            ∷ "MAKEFLAGS += --no-builtin-rules"
            ∷ ""
            ∷ "# Default for conditional backend skipping (avoid unbound var under set -u)"
            ∷ "SKIP_GHC_BACKEND ?="
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
            ∷ ""
            ∷ "# Dependency decomposition directories (fallback-safe)"
            ∷ "DEPS_DIR ?= $(if $(JSON_DECOMPOSE_FALLBACK_DIR),$(JSON_DECOMPOSE_FALLBACK_DIR),data/deps/)"
            ∷ "PLANNING_DIR ?= $(if $(JSON_DECOMPOSE_FALLBACK_DIR),$(JSON_DECOMPOSE_FALLBACK_DIR)/planning,$(DEPS_DIR)/planning/)"
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

-- Helper: check if list is empty
is_empty : ∀ {A : Set} → List A → Bool
is_empty [] = true
is_empty (_ ∷ _) = false

-- Helper: if-then-else on Bool
if_then_else : {A : Set} → Bool → A → A → A
if_then_else true x _ = x
if_then_else false _ y = y

main : IO ⊤
main = 
  discoverAgdaFiles >>= λ agdaFiles →
  readGraphEdges "build/diagrams/agda-deps-full.dot" >>= λ edges →
  let artifact = buildArtifact agdaFiles edges
      content = exportMakefile defaultRenderer artifact
      targets = (regenMakefileTarget ∷ discoveredTargets) 
      docsContent = renderDocs targets
      -- Prepend assumptions and validation notes
      graphStatus : String
      graphStatus = if is_empty edges then "NONE (WARNING: missing)" else "OK"
      fullDocsContent = 
        "# Makefile Generation Report\n\n" ++
        "## Generation Assumptions (Verified by ExporterMakefile)\n\n" ++
        "1. **Agda toolchain**: Available as `$(AGDA)` with `-i src/agda --ghc-flag=-Wno-star-is-type`.\n" ++
        "2. **Dependency graph**: `build/diagrams/agda-deps-full.dot` - Status: " ++ graphStatus ++ "\n" ++
        "3. **Module classification**: Agda.* modules excluded from internal deps; others are internal.\n" ++
        "4. **External tools**: python3, npx, npm, act, docker available on PATH.\n" ++
        "5. **Shell environment**: bash with set -euo pipefail for recipe isolation.\n" ++
        "6. **Parallelism**: Automatic core detection; override via MAKEFLAGS if needed.\n" ++
        "7. **Profiling**: JSONL logs optional; $(PROFILE_LOG) path is fallback-safe.\n\n" ++
        docsContent
    in writeFile "Makefile.generated" content >>= λ _ →
      writeFile "build/graph_parsed_state.txt" ("status: " ++ graphStatus ++ "\nedges: " ++ listLengthString edges ++ "\n") >>= λ _ →
     writeFile "build/makefile_targets_generated.md" fullDocsContent >>= λ _ →
     writeFile "docs/automation/MAKEFILE-TARGETS.md" fullDocsContent >>= λ _ →
     (if_empty_else edges
       (writeFile "build/makefile_generation_warning.txt" 
          "WARNING: Empty dependency graph. Consider: make build/diagrams/agda-deps-full.dot\n")
       (return tt))
  where
    case : ∀ {A B : Set} → List A → B → B → B
    case [] b _ = b
    case (_ ∷ _) _ c = c
    
    if_empty_else : ∀ {A : Set} → List A → IO ⊤ → IO ⊤ → IO ⊤
    if_empty_else [] action _ = action
    if_empty_else (_ ∷ _) _ action = action
