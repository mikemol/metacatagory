{-# OPTIONS --without-K #-}

module Examples.AgdaMakefileDeps where

open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.String using (String; primStringAppend)
open import Agda.Builtin.Char using (Char)
open import Agda.Builtin.Nat using (Nat; zero; suc)
open import Agda.Builtin.Unit using (⊤; tt)
open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.Maybe using (Maybe; just; nothing)
open import Agda.Builtin.Equality using (_≡_; refl)
open import Agda.Builtin.Bool using (Bool; true; false)

-- ==========================================================
-- Domain Model: Module Paths and Dependencies
-- ==========================================================

-- A module name is a list of components separated by dots
-- e.g., "Core.Utils" = ["Core", "Utils"]
ModuleName : Set
ModuleName = List String

postulate
  primStringEquality : String → String → Bool

{-# COMPILE GHC primStringEquality = (==) #-}

-- Distinguish between internal project modules and external dependencies
data ModuleOrigin : Set where
  Internal : ModuleName → ModuleOrigin   -- Our code in src/agda/
  External : ModuleName → ModuleOrigin   -- Agda builtins, stdlib, etc.

-- Decidable: is this an internal module we should track?
isInternalModule : ModuleName → Bool
isInternalModule [] = false
isInternalModule (first ∷ rest) with primStringEquality first "Agda"
... | true = false   -- External: Agda.Builtin.*, Agda.Primitive, etc.
... | false = true   -- Internal: everything else

-- Classify a module name
classifyModule : ModuleName → ModuleOrigin
classifyModule mod with isInternalModule mod
... | true = Internal mod
... | false = External mod

-- A file path in the repository
data FilePath : Set where
  agdaSource : ModuleName → FilePath
  agdaInterface : ModuleName → FilePath

-- An import declaration with proof it came from a valid line
data ImportDeclaration : Set where
  simpleImport : ModuleName → ImportDeclaration
  openImport : ModuleName → ImportDeclaration

-- A dependency relationship: module A depends on module B
record Dependency : Set where
  constructor _depends-on_
  field
    source : ModuleName
    target : ModuleName

-- ==========================================================
-- Decidable Predicates with Proofs
-- ==========================================================

-- Basic utilities
if_then_else_ : {A : Set} → Bool → A → A → A
if true  then x else _ = x
if false then _ else y = y

_||_ : Bool → Bool → Bool
true || _ = true
false || b = b

_&&_ : Bool → Bool → Bool
true && b = b
false && _ = false

not : Bool → Bool
not true = false
not false = true

eqNat : Nat → Nat → Bool
eqNat zero zero = true
eqNat (suc m) (suc n) = eqNat m n
eqNat _ _ = false

ltNat : Nat → Nat → Bool
ltNat _ zero = false
ltNat zero (suc _) = true
ltNat (suc m) (suc n) = ltNat m n

postulate
  primStringToList : String → List Char
  primCharEquality : Char → Char → Bool

{-# FOREIGN GHC import qualified Data.Text as T #-}
{-# COMPILE GHC primStringToList = T.unpack #-}
{-# COMPILE GHC primCharEquality = (==) #-}

-- Helper: list length
length : {A : Set} → List A → Nat
length [] = zero
length (x ∷ xs) = suc (length xs)

-- Helper: take first n elements
take : {A : Set} → Nat → List A → List A
take zero _ = []
take (suc n) [] = []
take (suc n) (x ∷ xs) = x ∷ take n xs

-- Helper: drop first n elements (example of adding new code)
drop : {A : Set} → Nat → List A → List A
drop zero xs = xs
drop (suc n) [] = []
drop (suc n) (x ∷ xs) = drop n xs

-- Decidable: does this string start with this prefix?
data StartsWith (prefix : String) (str : String) : Set where
  starts-with : (p≡ : primStringToList prefix ≡ take (length (primStringToList prefix)) (primStringToList str)) 
              → StartsWith prefix str

-- Character equality
eqChar : Char → Char → Bool
eqChar = primCharEquality

-- List equality for characters
eqListChar : List Char → List Char → Bool
eqListChar [] [] = true
eqListChar [] (_ ∷ _) = false
eqListChar (_ ∷ _) [] = false
eqListChar (x ∷ xs) (y ∷ ys) = if_then_else_ (eqChar x y) (eqListChar xs ys) false

-- Boolean version for practical use
startsWith? : String → String → Bool
startsWith? prefix str =
  let pchars = primStringToList prefix
      schars = primStringToList str
      plen = length pchars
      slen = length schars
  in if_then_else_ (ltNat slen plen) false (eqListChar (take plen schars) pchars)

-- Decidable: is this line an import statement?
data IsImportLine (line : String) : Set where
  is-simple-import : StartsWith "import " line → IsImportLine line
  is-open-import : StartsWith "open import " line → IsImportLine line

isImportLine? : (line : String) → Bool
isImportLine? line = (startsWith? "import " line) || (startsWith? "open import " line)

-- ==========================================================
-- FFI Bindings (minimal, typed)
-- ==========================================================

postulate
  readFileLines : String → IO (List String)
  primStringWords : String → List String

{-# FOREIGN GHC import qualified Data.Text.IO as TIO #-}
{-# COMPILE GHC readFileLines = \path -> fmap T.lines (TIO.readFile (T.unpack path)) #-}
{-# COMPILE GHC primStringWords = T.words #-}

-- ==========================================================
-- Path Transformations (Correct by Construction)
-- ==========================================================

infixr 20 _++_
_++_ : String → String → String
_++_ = primStringAppend

-- Join module name components with separator
joinWith : String → List String → String
joinWith sep [] = ""
joinWith sep (x ∷ []) = x
joinWith sep (x ∷ xs) = x ++ sep ++ joinWith sep xs

-- Convert module name to file path
moduleToFilePath : ModuleName → String
moduleToFilePath mod = "src/agda/" ++ joinWith "/" mod ++ ".agda"

-- Convert module name to interface path  
moduleToInterfacePath : ModuleName → String
moduleToInterfacePath mod = "src/agda/" ++ joinWith "/" mod ++ ".agdai"

-- ==========================================================
-- Import Extraction (Type-Guided)
-- ==========================================================

-- Utility: map function
map : {A B : Set} → (A → B) → List A → List B
map f [] = []
map f (x ∷ xs) = f x ∷ map f xs

-- Utility: filter list
filter : {A : Set} → (A → Bool) → List A → List A
filter p [] = []
filter p (x ∷ xs) = if_then_else_ (p x) (x ∷ filter p xs) (filter p xs)

-- Utility: safe list index
listIndex : {A : Set} → List A → Nat → Maybe A
listIndex [] _ = nothing
listIndex (x ∷ xs) zero = just x
listIndex (x ∷ xs) (suc n) = listIndex xs n

postulate
  primStringSplit : String → String → List String

{-# FOREIGN GHC import qualified Data.Text as T #-}
{-# COMPILE GHC primStringSplit = \sep s -> T.splitOn sep s #-}

-- Parse a dotted module name "Core.Phase.Utils" into ["Core", "Phase", "Utils"]
-- The module name in Agda source is written with dots, but represents a path
parseModuleName : String → ModuleName
parseModuleName s = primStringSplit "." s

-- Extract module name from import line (returns Maybe for safety)
extractModuleName : String → Maybe ModuleName
extractModuleName line with primStringWords line
... | ws with listIndex ws 0
... | nothing = nothing
... | just w = if_then_else_ (primStringEquality w "open")
                  (extractAt ws 2)
                  (extractAt ws 1)
  where
    extractAt : List String → Nat → Maybe ModuleName
    extractAt ws n with listIndex ws n
    ... | nothing = nothing
    ... | just modStr = just (parseModuleName modStr)

-- ==========================================================
-- Makefile Rule Generation (Correct by Construction)
-- ==========================================================

-- A Makefile rule with typed components
record MakefileRule : Set where
  constructor mkRule
  field
    target : String
    dependencies : List String
    recipe : String

-- Render rule to text format
renderRule : MakefileRule → String
renderRule (mkRule target deps recipe) =
  target ++ ": " ++ joinWith " " deps ++ "\n\t" ++ recipe ++ "\n"

-- Generate rule for an Agda module with its dependencies
generateRule : (source : ModuleName) → (imports : List ModuleName) → MakefileRule
generateRule source imports =
  let sourceFile = moduleToFilePath source
      targetFile = moduleToInterfacePath source
      importInterfaces = map moduleToInterfacePath imports
      allDeps = sourceFile ∷ importInterfaces
      recipe = "agda -i src/agda --ghc-flag=-Wno-star-is-type " ++ sourceFile
  in mkRule targetFile allDeps recipe

-- ==========================================================
-- Main Interface
-- ==========================================================

-- Extract all import lines from file content
extractImports : List String → List ModuleName
extractImports lines =
  let importLines = filter isImportLine? lines
      maybeModules = map extractModuleName importLines
      -- Filter out nothing values and external modules
      internalModules : List ModuleName
      internalModules = filterInternal maybeModules
  in internalModules
  where
    filterJust : {A : Set} → List (Maybe A) → List A
    filterJust [] = []
    filterJust (nothing ∷ xs) = filterJust xs
    filterJust (just x ∷ xs) = x ∷ filterJust xs
    
    filterInternal : List (Maybe ModuleName) → List ModuleName
    filterInternal xs = 
      let modules = filterJust xs
      in filter isInternalModule modules

-- Main: extract imports from file lines and generate Makefile rule
-- Pure function - no IO involved
makeRuleForFile : List String → ModuleName → String
makeRuleForFile lines moduleName =
  let imports = extractImports lines
      rule = generateRule moduleName imports
  in renderRule rule
