{-# OPTIONS --without-K #-}

module Examples.ExporterMakefile where

{-# FOREIGN GHC
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as M
import qualified Data.List as L
import           Data.Maybe (fromMaybe)
import qualified System.Directory as Dir

writeFileAdapter :: T.Text -> T.Text -> IO ()
writeFileAdapter path content = TIO.writeFile (T.unpack path) content

-- Parse DOT file edges into lines "src=>dst" using module labels
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

-- Convert a repo path like src/agda/Core/Utils.agda to module label Core.Utils
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
  environmentSetupToTarget; synchronizerToTarget; mkTarget)

postulate
  writeFile : String → String → IO ⊤
{-# COMPILE GHC writeFile = writeFileAdapter #-}

-- Section: Makefile Artifact Types
record MakefileSection : Set where
  field
    id : String
    content : List String

record MakefileArtifact : Set where
  field
    sections : List MakefileSection

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
  ; content = (MakefileTarget.name target ++ ": " ++ intercalate " " (MakefileTarget.dependencies target)) 
             ∷ map ("\t" ++_) (MakefileTarget.recipe target)
  }
  where
    intercalate : String → List String → String
    intercalate sep [] = ""
    intercalate sep (x ∷ []) = x
    intercalate sep (x ∷ xs) = x ++ sep ++ intercalate sep xs

regenMakefileSection : MakefileSection
regenMakefileSection = record
  { id = "regen-makefile"
  ; content = "# Use local Agda 2.8.0" ∷ "AGDA := .local/agda" ∷ "" ∷ "regen-makefile:" ∷ "\t$(AGDA) -i src/agda --compile --ghc-flag=-Wno-star-is-type src/agda/Examples/ExporterMakefile.agda && ./src/agda/ExporterMakefile" ∷ "\tcp Makefile.generated Makefile" ∷ []
  }

-- Discovered targets from category system
discoveredTargets : List MakefileTarget
discoveredTargets = 
  validatorToTarget "md-lint" "build/reports/md-lint.txt" 
    ("npx remark . --quiet --frail > build/reports/md-lint.txt" ∷ [])
  ∷ generatorToTarget "badges" ("build/reports/test-results.json" ∷ [])
    ("python3 scripts/generate-badges.py" ∷ [])
  ∷ environmentSetupToTarget "node-deps"
    ("npm install" ∷ [])
  ∷ generatorToTarget "deferred-items" ([])
    (".github/scripts/detect-deferred-items.sh" ∷ [])
  ∷ generatorToTarget "roadmap-index" ("src/agda/Plan/CIM/RoadmapIndex.agdai" ∷ [])
    ("$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/RoadmapIndex.agda" ∷ [])
  ∷ generatorToTarget "roadmap-sync" ("roadmap-export-json" ∷ "src/agda/Plan/CIM/RoadmapSync.agdai" ∷ [])
    ("$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/RoadmapSync.agda" ∷ [])
  ∷ generatorToTarget "roadmap-sppf" ("src/agda/Plan/CIM/RoadmapSPPF.agdai" ∷ [])
    ("$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/RoadmapSPPF.agda" ∷ [])
  ∷ generatorToTarget "roadmap-merge" ([])
    ("python3 scripts/merge_roadmaps.py" ∷ [])
  ∷ generatorToTarget "roadmap-deps-graph" ([])
    ("mkdir -p build/diagrams" ∷ "$(AGDA) --dependency-graph=build/diagrams/agda-deps-full.dot -i src/agda src/agda/Tests/Index.agda 2>&1 | grep -E \"(Checking|Error)\" | head -20" ∷ [])
  ∷ generatorToTarget "roadmap-enrich" ("build/canonical_roadmap.json" ∷ "build/diagrams/agda-deps-full.dot" ∷ [])
    ("python3 scripts/enrich_canonical.py" ∷ [])
  ∷ generatorToTarget "roadmap-export-json" ("build/canonical_roadmap.json" ∷ [])
    ("python3 scripts/export_canonical_json.py" ∷ [])
  ∷ generatorToTarget "roadmap-export-md" ("build/canonical_roadmap.json" ∷ [])
    ("python3 scripts/export_canonical_md.py" ∷ [])
  ∷ generatorToTarget "roadmap-export-enriched" ("build/canonical_enriched.json" ∷ [])
    ("python3 scripts/export_enriched_md.py" ∷ [])
  ∷ generatorToTarget "roadmap-export-deps" ("build/canonical_enriched.json" ∷ [])
    ("python3 scripts/export_dependency_graph.py" ∷ [])
  ∷ generatorToTarget "roadmap-validate-json" ("build/canonical_roadmap.json" ∷ ".github/roadmap/tasks.json" ∷ [])
    ("python3 scripts/validate_json.py" ∷ [])
  ∷ generatorToTarget "roadmap-validate-md" ("build/canonical_roadmap.json" ∷ "ROADMAP.md" ∷ [])
    ("python3 scripts/validate_md.py" ∷ [])
  ∷ generatorToTarget "roadmap-validate-triangle" ("roadmap-validate-json" ∷ "roadmap-validate-md" ∷ [])
    ("@echo \"✓ Triangle validation complete\"" ∷ [])
  ∷ generatorToTarget "roadmap-sppf-export" ("build/canonical_roadmap.json" ∷ [])
    ("python3 scripts/export_roadmap_sppf.py" ∷ [])
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
      recipe = ("$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type " ++ agdaPath) ∷ []
  in mkTarget agdaiPath allDeps recipe false

-- Build complete artifact from discovered files and graph edges
buildArtifact : List String → List String → MakefileArtifact
buildArtifact agdaFiles graphEdges =
  let labels = map pathToModuleLabel agdaFiles
      agdaiTargets = zipWith generateAgdaTargetFromGraph agdaFiles (map depsFor labels)
      docsTargets = generateDocsTargets agdaFiles
      aggregateTargets = allAgdaiTarget agdaFiles ∷ allDocsTarget agdaFiles ∷ []
      allTargets = discoveredTargets +++ agdaiTargets +++ docsTargets +++ aggregateTargets
      phonyNames = "all" ∷ "check" ∷ "md-fix" ∷ "md-lint" ∷ "badges" ∷ "node-deps" 
                 ∷ "regen-makefile" ∷ "agda-all" ∷ "docs-all" 
           ∷ "deferred-items" ∷ "roadmap-index" ∷ "roadmap-sync" ∷ "roadmap-sppf"
           ∷ "roadmap-merge" ∷ "roadmap-deps-graph" ∷ "roadmap-enrich"
           ∷ "roadmap-export-json" ∷ "roadmap-export-md" ∷ "roadmap-export-enriched"
           ∷ "roadmap-export-deps" ∷ "roadmap-validate-json" ∷ "roadmap-validate-md"
           ∷ "roadmap-validate-triangle" ∷ "roadmap-sppf-export" ∷ "roadmap-all-enriched"
                 ∷ []
      phonySection = record { id = "phony" 
                            ; content = (".PHONY: " ++ intercalate " " phonyNames) ∷ [] }
  in record { sections = regenMakefileSection ∷ phonySection ∷ map targetToSection allTargets }
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

-- IO-based main that discovers files and generates Makefile
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
  in writeFile "Makefile.generated" content
