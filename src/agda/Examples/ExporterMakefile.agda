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
open import Examples.Makefile.Targets.Python using (pythonTargets)
open import Examples.Makefile.Targets.RoadmapExports using (roadmapExportTargets)
open import Examples.Makefile.Targets.Docs using (docTargets)
open import Examples.Makefile.Targets.JsonRoundtrip using (jsonRoundtripTargets)
open import Examples.Makefile.Targets.Infra using (infraTargets)
open import Examples.Makefile.Targets.Priority using (priorityTargets)
open import Examples.Makefile.Targets.AgdaBuild using (agdaTargets)
open import Examples.Makefile.Targets.Composite using (compositeTargets)

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
  pythonTargets +++ roadmapExportTargets +++ docTargets +++ jsonRoundtripTargets +++ infraTargets +++ priorityTargets +++ agdaTargets +++ compositeTargets

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
            ∷ "# Shell configuration for error-safe recipes"
            ∷ "SHELL := /bin/bash"
            ∷ ".SHELLFLAGS := -euo pipefail -c"
            ∷ ".DELETE_ON_ERROR:"
            ∷ "MAKEFLAGS += --warn-undefined-variables"
            ∷ "MAKEFLAGS += --no-builtin-rules"
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
