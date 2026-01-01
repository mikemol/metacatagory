{-# OPTIONS --without-K #-}

-- | Example: scan files for deferred items (TODO/FIXME/etc.).
module Examples.DeferredItemsScanner where

open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.String using (String; primStringAppend)
open import Agda.Builtin.Nat using (Nat; _+_; zero; suc)
open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.Unit using (⊤; tt)
open import Agda.Builtin.Maybe using (Maybe; just; nothing)

{-# FOREIGN GHC
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.Process as Proc
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Format as TimeF
import qualified System.Environment

-- FFI Primitives for Core.IO
primBind :: IO a -> (a -> IO b) -> IO b
primBind = (>>=)

primThen :: IO a -> IO b -> IO b
primThen = (>>)

primReturn :: a -> IO a
primReturn = pure

primWriteFile :: T.Text -> T.Text -> IO ()
primWriteFile path content = TIO.writeFile (T.unpack path) content

primReadFile :: T.Text -> IO T.Text
primReadFile path = TIO.readFile (T.unpack path)

primAppendFile :: T.Text -> T.Text -> IO ()
primAppendFile path content = TIO.appendFile (T.unpack path) content

primPutStr :: T.Text -> IO ()
primPutStr = putStr . T.unpack

primPutStrLn :: T.Text -> IO ()
primPutStrLn = putStrLn . T.unpack

primGetLine :: IO T.Text
primGetLine = T.pack <$> getLine

-- FFI Primitive for Core.Strings
primNatToString :: Integer -> T.Text
primNatToString n = T.pack (show n)

-- Application-specific FFI
grepPatternAdapter :: T.Text -> T.Text -> IO [T.Text]
grepPatternAdapter pat label = do
  let cmd = "grep"
      args = ["-rn", "--include=*.agda", "--include=*.md", T.unpack pat, "src/", "testing.md", "README.md"]
  (exitCode, stdout, _) <- Proc.readProcessWithExitCode cmd args ""
  case exitCode of
    _ -> pure $ T.lines (T.pack stdout)

countItemsAdapter :: [T.Text] -> Integer
countItemsAdapter items = toInteger (length items)

getCurrentTimestampAdapter :: IO T.Text
getCurrentTimestampAdapter = do
  now <- Clock.getCurrentTime
  pure $ T.pack $ TimeF.formatTime TimeF.defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now

getGitHubOutputAdapter :: IO (Maybe T.Text)
getGitHubOutputAdapter = fmap T.pack <$> System.Environment.lookupEnv "GITHUB_OUTPUT"

appendGitHubOutputAdapter :: T.Text -> T.Text -> IO ()
appendGitHubOutputAdapter path content = 
  TIO.appendFile (T.unpack path) (T.append content (T.pack "\n"))
#-}

-- FFI postulates for shared module primitives
postulate
  _>>=_ : {A B : Set} → IO A → (A → IO B) → IO B
  _>>_ : {A B : Set} → IO A → IO B → IO B
  return : {A : Set} → A → IO A
  primWriteFile : String → String → IO ⊤
  primReadFile : String → IO String
  primAppendFile : String → String → IO ⊤
  primPutStr : String → IO ⊤
  primPutStrLn : String → IO ⊤
  primGetLine : IO String

{-# COMPILE GHC _>>=_ = \_ _ -> primBind #-}
{-# COMPILE GHC _>>_ = \_ _ -> primThen #-}
{-# COMPILE GHC return = \_ -> primReturn #-}
{-# COMPILE GHC primWriteFile = primWriteFile #-}
{-# COMPILE GHC primReadFile = primReadFile #-}
{-# COMPILE GHC primAppendFile = primAppendFile #-}
{-# COMPILE GHC primPutStr = primPutStr #-}
{-# COMPILE GHC primPutStrLn = primPutStrLn #-}
{-# COMPILE GHC primGetLine = primGetLine #-}
-- Open shared modules with FFI primitives
open import Core.Strings
open import Core.IO String ⊤ tt IO _>>=_ _>>_ return primWriteFile primReadFile primAppendFile primPutStr primPutStrLn primGetLine
open import Core.Rendering

-- Application-specific FFI postulates
postulate
  grepPattern : String → String → IO (List String)
  countItems : List String → Nat
  getCurrentTimestamp : IO String
  getGitHubOutput : IO (Maybe String)
  appendGitHubOutput : String → String → IO ⊤

{-# COMPILE GHC grepPattern = grepPatternAdapter #-}
{-# COMPILE GHC countItems = countItemsAdapter #-}
{-# COMPILE GHC getCurrentTimestamp = getCurrentTimestampAdapter #-}
{-# COMPILE GHC getGitHubOutput = getGitHubOutputAdapter #-}
{-# COMPILE GHC appendGitHubOutput = appendGitHubOutputAdapter #-}

-- Deferred item categories
data DeferredCategory : Set where
  DeviationLogCat : DeferredCategory
  PostulateCat : DeferredCategory
  TodoCat : DeferredCategory
  PlannedCat : DeferredCategory
  FixmeCat : DeferredCategory

record CategoryScan : Set where
  field
    category : DeferredCategory
    searchPattern : String
    label : String
    results : List String
    count : Nat

record DeferredSummary : Set where
  field
    deviationLog : Nat
    postulates : Nat
    todo : Nat
    planned : Nat
    fixme : Nat
    total : Nat
    timestamp : String

-- Get search pattern for category
categoryPattern : DeferredCategory → String
categoryPattern DeviationLogCat = "DeviationLog"
categoryPattern PostulateCat = "^[[:space:]]*postulate"
categoryPattern TodoCat = "TODO"
categoryPattern PlannedCat = "PLANNED"
categoryPattern FixmeCat = "FIXME"

-- Get label for category
categoryLabel : DeferredCategory → String
categoryLabel DeviationLogCat = "DeviationLog Entries"
categoryLabel PostulateCat = "Postulates"
categoryLabel TodoCat = "TODO Items"
categoryLabel PlannedCat = "PLANNED Items"
categoryLabel FixmeCat = "FIXME Items"

-- All categories
allDeferredCategories : List DeferredCategory
allDeferredCategories = DeviationLogCat ∷ PostulateCat ∷ TodoCat ∷ PlannedCat ∷ FixmeCat ∷ []

-- Scan single category
scanCategory : DeferredCategory → IO CategoryScan
scanCategory cat = do
  let pat = categoryPattern cat
  let lbl = categoryLabel cat
  results ← grepPattern pat lbl
  return record
    { category = cat
    ; searchPattern = pat
    ; label = lbl
    ; results = results
    ; count = countItems results
    }

-- Scan all categories
scanAllCategories : List DeferredCategory → IO (List CategoryScan)
scanAllCategories [] = return []
scanAllCategories (cat ∷ cats) = do
  scan ← scanCategory cat
  rest ← scanAllCategories cats
  return (scan ∷ rest)

-- Build markdown section from scan
scanToMarkdownSection : CategoryScan → MarkdownSection
scanToMarkdownSection scan =
  record
    { heading = CategoryScan.label scan
    ; content = formatResults (CategoryScan.results scan) (CategoryScan.count scan)
    }
  where
    mapItems : List String → List String
    mapItems [] = []
    mapItems (x ∷ xs) = ("- `" ++ (x ++ "`")) ∷ mapItems xs
    
    formatResults : List String → Nat → List String
    formatResults [] _ = "✅ No items found." ∷ []
    formatResults items count = 
      ("Found **" ++ (natToString count ++ "** instances:"))
      ∷ ""
      ∷ mapItems items

-- Build summary section
summaryToMarkdownSection : DeferredSummary → MarkdownSection
summaryToMarkdownSection summary =
  record
    { heading = "Summary"
    ; content =
        "| Category | Count |"
        ∷ "|----------|-------|"
        ∷ ("| DeviationLog | " ++ (natToString (DeferredSummary.deviationLog summary) ++ " |"))
        ∷ ("| Postulates | " ++ (natToString (DeferredSummary.postulates summary) ++ " |"))
        ∷ ("| TODO | " ++ (natToString (DeferredSummary.todo summary) ++ " |"))
        ∷ ("| PLANNED | " ++ (natToString (DeferredSummary.planned summary) ++ " |"))
        ∷ ("| FIXME | " ++ (natToString (DeferredSummary.fixme summary) ++ " |"))
        ∷ ("| **Total** | **" ++ (natToString (DeferredSummary.total summary) ++ "** |"))
        ∷ []
    }

-- Build markdown document
buildMarkdownDocument : DeferredSummary → List CategoryScan → MarkdownDocument
buildMarkdownDocument summary scans =
  record
    { title = "Deferred Items Report"
    ; metadata = ("Generated on: " ++ DeferredSummary.timestamp summary) ∷ []
    ; sections = mapSections scans (summaryToMarkdownSection summary ∷ [])
    }
  where
    mapSections : List CategoryScan → List MarkdownSection → List MarkdownSection
    mapSections [] acc = acc
    mapSections (x ∷ xs) acc = scanToMarkdownSection x ∷ mapSections xs acc

-- Build JSON object
summaryToJSON : DeferredSummary → JSONObject
summaryToJSON summary =
  record
    { fields =
        jsonNumberField "total" (DeferredSummary.total summary)
        ∷ jsonNumberField "deviation_log" (DeferredSummary.deviationLog summary)
        ∷ jsonNumberField "postulates" (DeferredSummary.postulates summary)
        ∷ jsonNumberField "todo" (DeferredSummary.todo summary)
        ∷ jsonNumberField "planned" (DeferredSummary.planned summary)
        ∷ jsonNumberField "fixme" (DeferredSummary.fixme summary)
        ∷ jsonStringField "timestamp" (DeferredSummary.timestamp summary)
        ∷ []
    }

-- Build summary from scans
buildSummary : String → List CategoryScan → DeferredSummary
buildSummary timestamp scans =
  record
    { deviationLog = getCountFor DeviationLogCat scans
    ; postulates = getCountFor PostulateCat scans
    ; todo = getCountFor TodoCat scans
    ; planned = getCountFor PlannedCat scans
    ; fixme = getCountFor FixmeCat scans
    ; total = sumCounts scans
    ; timestamp = timestamp
    }
  where
    eqCategory : DeferredCategory → DeferredCategory → Bool
    eqCategory DeviationLogCat DeviationLogCat = true
    eqCategory PostulateCat PostulateCat = true
    eqCategory TodoCat TodoCat = true
    eqCategory PlannedCat PlannedCat = true
    eqCategory FixmeCat FixmeCat = true
    eqCategory _ _ = false
    
    getCountFor : DeferredCategory → List CategoryScan → Nat
    getCountFor cat [] = zero
    getCountFor cat (scan ∷ rest) with eqCategory cat (CategoryScan.category scan)
    ... | true = CategoryScan.count scan
    ... | false = getCountFor cat rest
    
    sumCounts : List CategoryScan → Nat
    sumCounts [] = zero
    sumCounts (scan ∷ rest) = CategoryScan.count scan + sumCounts rest

-- Write GitHub Actions output
writeGitHubOutputs : DeferredSummary → Maybe String → IO ⊤
writeGitHubOutputs summary nothing = return tt
writeGitHubOutputs summary (just outputPath) = do
  appendGitHubOutput outputPath ("total=" ++ natToString (DeferredSummary.total summary))
  appendGitHubOutput outputPath ("deviation_log=" ++ natToString (DeferredSummary.deviationLog summary))
  appendGitHubOutput outputPath ("postulates=" ++ natToString (DeferredSummary.postulates summary))
  appendGitHubOutput outputPath ("todo=" ++ natToString (DeferredSummary.todo summary))
  appendGitHubOutput outputPath ("planned=" ++ natToString (DeferredSummary.planned summary))
  appendGitHubOutput outputPath ("fixme=" ++ natToString (DeferredSummary.fixme summary))
  return tt

-- Main scanner
runDeferredItemsScanner : IO ⊤
runDeferredItemsScanner = do
  timestamp ← getCurrentTimestamp
  scans ← scanAllCategories allDeferredCategories
  let summary = buildSummary timestamp scans
  let markdownDoc = buildMarkdownDocument summary scans
  let jsonObj = summaryToJSON summary
  let markdown = renderMarkdownDocument markdownDoc
  let json = renderJSONObject jsonObj
  writeFile "deferred-items.md" markdown
  writeFile "deferred-summary.json" json
  mOutput ← getGitHubOutput
  writeGitHubOutputs summary mOutput
  return tt

-- Executable main
main : IO ⊤
main = runDeferredItemsScanner
