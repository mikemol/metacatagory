{-# OPTIONS --without-K #-}

-- | Example: simple makefile target emission from Agda lists.
module Examples.MakefileTargets where

open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.String using (String; primStringAppend; primStringEquality; primStringToList; primStringFromList)
open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.Maybe using (Maybe; just; nothing)
open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.Char using (Char; primCharEquality)

infixr 20 _++_
_++_ : String → String → String
_++_ = primStringAppend

-- Minimal map (to avoid pulling stdlib).
map : ∀ {A B : Set} → (A → B) → List A → List B
map f [] = []
map f (x ∷ xs) = f x ∷ map f xs

-- Join a list of commands with separators (used to build timing wrappers).
joinWith : String → List String → String
joinWith sep [] = ""
joinWith sep (x ∷ []) = x
joinWith sep (x ∷ xs) = x ++ sep ++ joinWith sep xs

-- Recipe scripts live under build/recipes except for regen-makefile.
recipeScriptPath : String → String
recipeScriptPath n with primStringEquality n "regen-makefile"
... | true = "scripts/recipes/regen-makefile.sh"
... | false = "build/recipes/" ++ n ++ ".sh"

escapeSingleQuotes : String → String
escapeSingleQuotes s = primStringFromList (escapeChars (primStringToList s))
  where
    escapeChars : List Char → List Char
    escapeChars [] = []
    escapeChars ('\'' ∷ xs) = '\'' ∷ '\\' ∷ '\'' ∷ '\'' ∷ escapeChars xs
    escapeChars (c ∷ xs) = c ∷ escapeChars xs

-- Profiling wrapper injected into Makefile recipes.
profileRecipe : String → List String
profileRecipe name =
  let escapedName = escapeSingleQuotes name
      escapedPath = escapeSingleQuotes (recipeScriptPath name)
  in ("scripts/run_profiled.sh '" ++ escapedName ++ "' '" ++ escapedPath ++ "'") ∷ []

-- Store raw recipe lines (profiling wrapper is injected when rendering the Makefile).
instrumentRecipe : String → List String → List String
instrumentRecipe _ cmds = cmds

startsWith? : String → String → Bool
startsWith? prefix str = startsWithChars (primStringToList prefix) (primStringToList str)
  where
    startsWithChars : List Char → List Char → Bool
    startsWithChars [] _ = true
    startsWithChars (_ ∷ _) [] = false
    startsWithChars (p ∷ ps) (s ∷ ss) with primCharEquality p s
    ... | true = startsWithChars ps ss
    ... | false = false

-- ==========================================================
-- Domain Model: What Transformations Exist?
-- ==========================================================

-- File patterns for matching
-- | Describe a file glob to search (extension, directory, recursion flag).
record FilePattern : Set where
  constructor mkPattern
  field
    extension : String     -- e.g., ".agda", ".md"
    directory : String     -- e.g., "src/agda", "."
    recursive : Bool       -- search subdirectories?

-- Source and output specifications
-- | Source type for a transformation pipeline.
data SourcePattern : Set where
  agdaSource : FilePattern → SourcePattern
  markdownSource : FilePattern → SourcePattern
  anySource : FilePattern → SourcePattern

-- | Output artifact category produced by a pipeline.
data OutputPattern : Set where
  agdaInterface : String → OutputPattern    -- .agdai files
  htmlDoc : String → OutputPattern          -- HTML documentation
  validationReport : String → OutputPattern -- Lint/check results
  artifact : String → OutputPattern         -- Generated files (badges, etc.)

-- External data sources
-- | Side inputs that may be pulled during target execution.
data DataSource : Set where
  fileMetadata : String → DataSource        -- Read from files
  gitInfo : DataSource                      -- Git repository state
  testResults : DataSource                  -- Test execution results
  apiQuery : String → DataSource            -- External API

-- API endpoints for synchronization
-- | Remote APIs we sync with.
data RemoteAPI : Set where
  githubIssues : String → String → RemoteAPI  -- owner/repo
  githubProjects : String → String → RemoteAPI

-- Environment requirements
-- | Environment prerequisites before running a target.
data Requirement : Set where
  nodeModules : Requirement
  pythonVenv : Requirement
  agdaLibraries : Requirement

-- ==========================================================
-- Target Categories: Typed Transformations
-- ==========================================================

-- | Typed shape of a target (transform, validate, generate, sync, setup).
data TargetCategory : Set where
  FileTransform : SourcePattern → OutputPattern → List String → TargetCategory
  Validator : FilePattern → String → TargetCategory
  Generator : DataSource → String → List String → TargetCategory
  Synchronizer : String → RemoteAPI → List String → TargetCategory
  EnvironmentSetup : Requirement → List String → TargetCategory

-- ==========================================================
-- Makefile Target Representation
-- ==========================================================

-- | Mutability classification for targets (graded).
data Mutability : Set where
  ReadOnly : Mutability
  ReportOnly : Mutability
  BuildOnly : Mutability
  RepoWrite : Mutability

-- | Certificate required to construct mutating targets.
record MutateCert : Set where
  constructor mkMutateCert
  field
    reason : String
    provenance : String
    scope : String

mutateCert : MutateCert
mutateCert = mkMutateCert "makefile-targets" "Examples.MakefileTargets" "build"

-- | Concrete make target descriptor with dependencies and recipe lines.
record MakefileTarget : Set where
  constructor mkTarget
  field
    name : String
    description : String  -- enforced documentation
    dependencies : List String
    recipe : List String
    phony : Bool
    mutability : Mutability
    scope : String

-- Explicit constructors to enforce mutability certificates where needed.
mkReadOnlyTarget : String → String → List String → List String → Bool → MakefileTarget
mkReadOnlyTarget name description deps recipe phony =
  mkTarget name description deps recipe phony ReadOnly ""

mutabilityForTargetName : String → Mutability
mutabilityForTargetName name with startsWith? "build/reports/" name
... | true = ReportOnly
... | false with startsWith? "build/reports/profiles.d/" name
...   | true = ReportOnly
...   | false with startsWith? "build/" name
...     | true = BuildOnly
...     | false = RepoWrite

scopeForTargetName : String → String
scopeForTargetName name with startsWith? "build/reports/" name
... | true = "reports"
... | false with startsWith? "build/reports/profiles.d/" name
...   | true = "reports"
...   | false with startsWith? "build/" name
...     | true = "build"
...     | false with startsWith? "data/" name
...       | true = "data"
...       | false with startsWith? "docs/" name
...         | true = "docs"
...         | false with startsWith? ".github/" name
...           | true = "github"
...           | false with primStringEquality name "ROADMAP.md"
...             | true = "docs"
...             | false with primStringEquality name "README.md"
...               | true = "docs"
...               | false with primStringEquality name "NAVIGATION.md"
...                 | true = "docs"
...                 | false with primStringEquality name "CONTRIBUTING.md"
...                   | true = "docs"
...                   | false = "repo"

mkMutativeTarget : MutateCert → String → String → List String → List String → Bool → MakefileTarget
mkMutativeTarget cert name description deps recipe phony =
  mkTarget name description deps recipe phony (mutabilityForTargetName name) (scopeForTargetName name)

-- ==========================================================
-- Category to Target Conversion
-- ==========================================================

-- Convert a source file path to output path based on pattern
transformPath : SourcePattern → OutputPattern → String → String
transformPath (agdaSource _) (agdaInterface _) path = 
  replaceExtension path ".agda" ".agdai"
  where
    replaceExtension : String → String → String → String
    replaceExtension path old new = path ++ ".agdai"  -- simplified for now
transformPath (agdaSource _) (htmlDoc dir) path = 
  dir ++ "/" ++ path ++ ".html"
transformPath (markdownSource _) (validationReport _) path = 
  "build/reports/md-lint.txt"
transformPath _ _ path = path

-- Generate recipe for transformation
generateRecipe : SourcePattern → OutputPattern → String → List String
generateRecipe (agdaSource _) (agdaInterface _) sourcePath =
  ("$(AGDA) $(AGDA_FLAGS) " ++ sourcePath) ∷ []
generateRecipe (agdaSource _) (htmlDoc _) sourcePath =
  ("agda --html --html-dir=build/html -i src/agda " ++ sourcePath) ∷ []
generateRecipe (markdownSource _) (validationReport _) _ =
  "npx remark . --quiet --frail > build/reports/md-lint.txt" ∷ []
generateRecipe _ _ _ = []

-- Convert FileTransform category to concrete target
fileTransformToTarget : MutateCert → String → String → SourcePattern → OutputPattern → List String → MakefileTarget
fileTransformToTarget cert sourcePath targetPath sourcePattern outputPattern extraDeps =
  mkMutativeTarget cert
    targetPath 
    ("Compile " ++ sourcePath ++ " to " ++ targetPath)
    (sourcePath ∷ extraDeps)
    (instrumentRecipe targetPath (generateRecipe sourcePattern outputPattern sourcePath))
    false

-- Convert Validator to target
validatorToTarget : String → String → String → List String → List String → MakefileTarget
validatorToTarget name description outputFile deps recipe =
  mkTarget name description deps (instrumentRecipe name recipe) true
    (mutabilityForTargetName outputFile)
    (scopeForTargetName outputFile)

-- Convert Generator to target
generatorToTarget : MutateCert → String → String → List String → List String → MakefileTarget
generatorToTarget cert name description deps recipe =
  mkMutativeTarget cert name description deps (instrumentRecipe name recipe) true

-- Convert Generator to file-producing target (non-phony for incremental builds)
generatorToFileTarget : MutateCert → String → String → List String → List String → MakefileTarget
generatorToFileTarget cert name description deps recipe =
  mkMutativeTarget cert name description deps (instrumentRecipe name recipe) false

-- Convert EnvironmentSetup to target
environmentSetupToTarget : MutateCert → String → String → List String → MakefileTarget
environmentSetupToTarget cert name description recipe =
  mkMutativeTarget cert name description [] (instrumentRecipe name recipe) true

-- Convert Synchronizer to target
synchronizerToTarget : MutateCert → String → String → List String → List String → MakefileTarget
synchronizerToTarget cert name description deps recipe =
  mkMutativeTarget cert name description deps (instrumentRecipe name recipe) true

-- ==========================================================
-- Discovery and Generation
-- ==========================================================

-- Helper: convert .agda path to .agdai path
agdaToAgdai : String → String
agdaToAgdai path = path ++ "i"  -- simple append for now

-- Generate targets for discovered Agda files
generateAgdaTargets : MutateCert → List String → List MakefileTarget
generateAgdaTargets cert [] = []
generateAgdaTargets cert (path ∷ paths) =
  let target = agdaToAgdai path
      recipe = ("$(AGDA) $(AGDA_FLAGS) " ++ path) ∷ []
  in mkMutativeTarget cert target ("Compile " ++ path) (path ∷ []) (instrumentRecipe target recipe) false ∷ generateAgdaTargets cert paths

-- Generate HTML documentation targets
generateDocsTargets : MutateCert → List String → List MakefileTarget  
generateDocsTargets cert [] = []
generateDocsTargets cert (path ∷ paths) =
  let htmlTarget = "build/html/" ++ path ++ ".html"
      agdaiDep = agdaToAgdai path
      recipe = ("$(AGDA) --html --html-dir=build/html -i src/agda " ++ path) ∷ []
  in mkMutativeTarget cert htmlTarget ("Generate HTML for " ++ path) (agdaiDep ∷ []) (instrumentRecipe htmlTarget recipe) false ∷ generateDocsTargets cert paths

-- Aggregate target: build all .agdai files
allAgdaiTarget : MutateCert → List String → MakefileTarget
allAgdaiTarget cert agdaFiles =
  let agdaiFiles = map agdaToAgdai agdaFiles
  in mkMutativeTarget cert "agda-all" "Compile all Agda modules" agdaiFiles (instrumentRecipe "agda-all" []) true

-- Aggregate target: build all HTML docs
allDocsTarget : MutateCert → List String → MakefileTarget
allDocsTarget cert agdaFiles =
  let htmlFiles = map (\path → "build/html/" ++ path ++ ".html") agdaFiles
  in mkMutativeTarget cert "docs-all" "Generate all HTML documentation" htmlFiles (instrumentRecipe "docs-all" []) true

-- ==========================================================
-- Discovery Functions
-- ==========================================================

postulate
  discoverAgdaFiles : IO (List String)
  discoverMarkdownFiles : IO (List String)

{-# FOREIGN GHC
import qualified System.Directory as Dir
import qualified System.FilePath as FP
import qualified Data.Text as T

findFilesByExt :: FilePath -> String -> IO [T.Text]
findFilesByExt dir ext = do
  exists <- Dir.doesDirectoryExist dir
  if not exists then return []
  else do
    contents <- Dir.listDirectory dir
    files <- fmap concat $ mapM (\name -> do
      let path = dir FP.</> name
      isDir <- Dir.doesDirectoryExist path
      if isDir && name /= "." && name /= ".." && not (isPrefixOf "." name)
        then findFilesByExt path ext
        else return $ if FP.takeExtension path == ext
                      then [T.pack path]
                      else []
      ) contents
    return files
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
#-}

{-# COMPILE GHC discoverAgdaFiles = findFilesByExt "src/agda" ".agda" #-}
{-# COMPILE GHC discoverMarkdownFiles = findFilesByExt "." ".md" #-}

-- ==========================================================
-- Known Target Categories (to be discovered)
-- ==========================================================

-- Documentation generation
docsCategory : TargetCategory
docsCategory = FileTransform 
  (agdaSource (mkPattern ".agda" "src/agda" true))
  (htmlDoc "build/html")
  []

-- Markdown validation
mdLintCategory : TargetCategory
mdLintCategory = Validator
  (mkPattern ".md" "." true)
  "Lint markdown files using markdownlint"

-- Badge generation
badgeCategory : TargetCategory  
badgeCategory = Generator
  testResults
  "Generate coverage badges"
  ("python3 scripts/generate-badges.py" ∷ [])

-- Node modules setup
nodeSetupCategory : TargetCategory
nodeSetupCategory = EnvironmentSetup
  nodeModules
  ("npm install" ∷ [])

-- Deferred items tracking
deferredItemsCategory : TargetCategory
deferredItemsCategory = Generator
  (fileMetadata "src/")
  "Scan and report deferred items (TODO, FIXME)"
  ("./src/agda/DeferredItemsScanner" ∷ [])

-- Roadmap sync with GitHub issues  
roadmapSyncCategory : TargetCategory
roadmapSyncCategory = Synchronizer
  "Sync roadmap with GitHub issues"
  (githubIssues "owner" "repo")
  ("./src/agda/RoadmapIssueSync" ∷ [])

-- JSON decomposition: monolithic → hierarchical
jsonDecomposeCategory : TargetCategory
jsonDecomposeCategory = Generator
  (fileMetadata "build/")
  "Decompose monolithic JSON to hierarchical structure"
  ("python3 scripts/json_decompose.py" ∷ [])

-- JSON recomposition: hierarchical → monolithic  
jsonRecomposeCategory : TargetCategory
jsonRecomposeCategory = Generator
  (fileMetadata "build/")
  "Recompose hierarchical JSON to monolithic form"
  ("python3 scripts/json_recompose.py" ∷ [])

-- Export all categories for discovery
allCategories : List TargetCategory
allCategories = 
  docsCategory ∷
  mdLintCategory ∷
  badgeCategory ∷
  nodeSetupCategory ∷
  deferredItemsCategory ∷
  roadmapSyncCategory ∷
  jsonDecomposeCategory ∷
  jsonRecomposeCategory ∷
  []
