{-# OPTIONS --without-K #-}

module Examples.MakefileTargets where

open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.String using (String; primStringAppend; primStringEquality)
open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.Maybe using (Maybe; just; nothing)
open import Agda.Builtin.IO using (IO)

infixr 20 _++_
_++_ : String → String → String
_++_ = primStringAppend

-- ==========================================================
-- Domain Model: What Transformations Exist?
-- ==========================================================

-- File patterns for matching
record FilePattern : Set where
  constructor mkPattern
  field
    extension : String     -- e.g., ".agda", ".md"
    directory : String     -- e.g., "src/agda", "."
    recursive : Bool       -- search subdirectories?

-- Source and output specifications
data SourcePattern : Set where
  agdaSource : FilePattern → SourcePattern
  markdownSource : FilePattern → SourcePattern
  anySource : FilePattern → SourcePattern

data OutputPattern : Set where
  agdaInterface : String → OutputPattern    -- .agdai files
  htmlDoc : String → OutputPattern          -- HTML documentation
  validationReport : String → OutputPattern -- Lint/check results
  artifact : String → OutputPattern         -- Generated files (badges, etc.)

-- External data sources
data DataSource : Set where
  fileMetadata : String → DataSource        -- Read from files
  gitInfo : DataSource                      -- Git repository state
  testResults : DataSource                  -- Test execution results
  apiQuery : String → DataSource            -- External API

-- API endpoints for synchronization
data RemoteAPI : Set where
  githubIssues : String → String → RemoteAPI  -- owner/repo
  githubProjects : String → String → RemoteAPI

-- Environment requirements
data Requirement : Set where
  nodeModules : Requirement
  pythonVenv : Requirement
  agdaLibraries : Requirement

-- ==========================================================
-- Target Categories: Typed Transformations
-- ==========================================================

data TargetCategory : Set where
  FileTransform : SourcePattern → OutputPattern → List String → TargetCategory
  Validator : FilePattern → String → TargetCategory
  Generator : DataSource → String → List String → TargetCategory
  Synchronizer : String → RemoteAPI → List String → TargetCategory
  EnvironmentSetup : Requirement → List String → TargetCategory

-- ==========================================================
-- Makefile Target Representation
-- ==========================================================

record MakefileTarget : Set where
  constructor mkTarget
  field
    name : String
    dependencies : List String
    recipe : List String
    phony : Bool

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
  ("agda -i src/agda --ghc-flag=-Wno-star-is-type " ++ sourcePath) ∷ []
generateRecipe (agdaSource _) (htmlDoc _) sourcePath =
  ("agda --html --html-dir=build/html -i src/agda " ++ sourcePath) ∷ []
generateRecipe (markdownSource _) (validationReport _) _ =
  "npx remark . --quiet --frail > build/reports/md-lint.txt" ∷ []
generateRecipe _ _ _ = []

-- Convert FileTransform category to concrete target
fileTransformToTarget : String → String → SourcePattern → OutputPattern → List String → MakefileTarget
fileTransformToTarget sourcePath targetPath sourcePattern outputPattern extraDeps =
  mkTarget 
    targetPath 
    (sourcePath ∷ extraDeps)
    (generateRecipe sourcePattern outputPattern sourcePath)
    false

-- Convert Validator to target
validatorToTarget : String → String → List String → MakefileTarget
validatorToTarget name outputFile recipe =
  mkTarget name [] recipe true

-- Convert Generator to target
generatorToTarget : String → List String → List String → MakefileTarget
generatorToTarget name deps recipe =
  mkTarget name deps recipe true

-- Convert EnvironmentSetup to target
environmentSetupToTarget : String → List String → MakefileTarget
environmentSetupToTarget name recipe =
  mkTarget name [] recipe true

-- ==========================================================
-- Discovery and Generation
-- ==========================================================

-- Helper: convert .agda path to .agdai path
agdaToAgdai : String → String
agdaToAgdai path = path ++ "i"  -- simple append for now

-- Generate targets for discovered Agda files
generateAgdaTargets : List String → List MakefileTarget
generateAgdaTargets [] = []
generateAgdaTargets (path ∷ paths) =
  let target = agdaToAgdai path
      recipe = ("$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type " ++ path) ∷ []
  in mkTarget target (path ∷ []) recipe false ∷ generateAgdaTargets paths

-- Generate HTML documentation targets
generateDocsTargets : List String → List MakefileTarget  
generateDocsTargets [] = []
generateDocsTargets (path ∷ paths) =
  let htmlTarget = "build/html/" ++ path ++ ".html"
      agdaiDep = agdaToAgdai path
      recipe = ("$(AGDA) --html --html-dir=build/html -i src/agda " ++ path) ∷ []
  in mkTarget htmlTarget (agdaiDep ∷ []) recipe false ∷ generateDocsTargets paths

-- Aggregate target: build all .agdai files
allAgdaiTarget : List String → MakefileTarget
allAgdaiTarget agdaFiles =
  let agdaiFiles = map agdaToAgdai agdaFiles
  in mkTarget "agda-all" agdaiFiles [] true
  where
    map : (String → String) → List String → List String
    map f [] = []
    map f (x ∷ xs) = f x ∷ map f xs

-- Aggregate target: build all HTML docs
allDocsTarget : List String → MakefileTarget
allDocsTarget agdaFiles =
  let htmlFiles = map (\path → "build/html/" ++ path ++ ".html") agdaFiles
  in mkTarget "docs-all" htmlFiles [] true
  where
    map : (String → String) → List String → List String
    map f [] = []
    map f (x ∷ xs) = f x ∷ map f xs

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
  "build/reports/md-lint.txt"

-- Badge generation
badgeCategory : TargetCategory  
badgeCategory = Generator
  testResults
  "build/badges/coverage.svg"
  ("python3 scripts/generate-badges.py" ∷ [])

-- Node modules setup
nodeSetupCategory : TargetCategory
nodeSetupCategory = EnvironmentSetup
  nodeModules
  ("npm install" ∷ [])

-- Export all categories for discovery
allCategories : List TargetCategory
allCategories = 
  docsCategory ∷
  mdLintCategory ∷
  badgeCategory ∷
  nodeSetupCategory ∷
  []
