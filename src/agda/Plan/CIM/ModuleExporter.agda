{-# OPTIONS --without-K #-}

module Plan.CIM.ModuleExporter where

open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.Unit using (⊤; tt)
open import Agda.Builtin.String using (String)
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.Bool using (Bool; true; false)
open import Core.Utils using (map)

{-# FOREIGN GHC
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.Directory as Dir
import qualified System.FilePath as FP
import           Data.List (isPrefixOf, nub, sortBy, isSuffixOf)
import           Data.Ord (comparing)
import           Data.Char (isSpace)
import           Control.Exception (catch, SomeException)

-- Extract module name from file path: src/agda/Core/Utils.agda -> Core.Utils
extractModuleName :: FilePath -> T.Text
extractModuleName p =
  let p'  = if "src/agda/" `isPrefixOf` p then drop 9 p else p
      noE = if ".agda" `isSuffixOf` p' then take (length p' - 5) p' else p'
  in T.pack $ map (\c -> if c == '/' then '.' else c) noE

-- Find all .agda files recursively
findAgdaFilesRec :: FilePath -> IO [FilePath]
findAgdaFilesRec dir = do
  exists <- Dir.doesDirectoryExist dir
  if not exists then return []
  else do
    (contents :: [String]) <- Dir.listDirectory dir `catch` \(_ :: SomeException) -> return []
    results <- mapM processEntry contents
    return $ concat results
  where
    processEntry name = do
      let path = dir FP.</> name
      isDir <- Dir.doesDirectoryExist path `catch` \(_ :: SomeException) -> return False
      if isDir && not ('.' `elem` name) && name /= "MAlonzo" && name /= ".git"
        then findAgdaFilesRec path  -- recursive call for directories
        else if ".agda" `isSuffixOf` path
             then return [path]       -- return agda file
             else return []           -- skip non-agda files

-- Extract imports from agda file
extractImports :: T.Text -> [T.Text]
extractImports content =
  let ls = T.lines content
      stripText t = T.dropWhile isSpace (dropWhileEnd isSpace t)
      dropWhileEnd p t = T.reverse (T.dropWhile p (T.reverse t))
      isImport l = T.isPrefixOf (T.pack "import ") (stripText l) ||
                   T.isPrefixOf (T.pack "open import ") (stripText l)
      importLines = filter isImport ls
  in nub $ map (stripText . T.drop 7 . T.dropWhile (/= 'i')) importLines

-- Generate markdown with YAML frontmatter
generateModuleMarkdown :: T.Text -> [T.Text] -> T.Text
generateModuleMarkdown modName imports =
  let frontmatter = T.unlines $
        [ T.pack "---"
        , T.pack "module: " `T.append` modName
        , T.pack "kind: per-module"
        , T.pack "imports:"
        ] ++ map (\imp -> T.pack "  - " `T.append` imp) imports ++
        [ T.pack "---" ]
      content = T.unlines
        [ T.pack ""
        , T.pack "# Module: " `T.append` modName
        , T.pack ""
        , T.pack "**Source:** `src/agda/" `T.append` T.replace (T.pack ".") (T.pack "/") modName `T.append` T.pack ".agda`"
        , T.pack ""
        , T.pack "## Dependencies"
        , T.pack ""
        ]
      importSection = if null imports
        then T.pack "*No imports*\n\n"
        else T.unlines $ map (\imp -> T.pack "* " `T.append` imp) imports
  in T.concat [frontmatter, content, importSection]

-- Write module markdown to build/md/modules/
writeModuleMarkdown :: T.Text -> T.Text -> IO ()
writeModuleMarkdown modName markdown = do
  let outFile = "build/md/modules/" ++ T.unpack modName ++ ".md"
      outDir = FP.takeDirectory outFile
  Dir.createDirectoryIfMissing True outDir
  TIO.writeFile outFile markdown
  putStrLn $ "✓ Generated " ++ outFile

-- Process all agda files
processAllAgdaFiles :: [FilePath] -> IO ()
processAllAgdaFiles [] = putStrLn "✓ Module documentation generated"
processAllAgdaFiles (filepath:rest) = do
  content <- TIO.readFile filepath `catch` \(_ :: SomeException) -> return (T.pack "")
  let modName = extractModuleName filepath
      imports = extractImports content
      markdown = generateModuleMarkdown modName imports
  writeModuleMarkdown modName markdown
  processAllAgdaFiles rest

#-}

postulate
  findAgdaFilesFFI : String → IO (List String)
  processAllAgdaFilesFFI : List String → IO ⊤
  putStrLn : String → IO ⊤

{-# COMPILE GHC findAgdaFilesFFI = \path -> fmap (map T.pack) (findAgdaFilesRec (T.unpack path)) #-}
{-# COMPILE GHC processAllAgdaFilesFFI = processAllAgdaFiles . map T.unpack #-}
{-# COMPILE GHC putStrLn = \s -> putStrLn (T.unpack s) #-}

postulate
  _>>=_ : {A B : Set} → IO A → (A → IO B) → IO B
  return : {A : Set} → A → IO A

{-# COMPILE GHC _>>=_ = \_ _ -> (>>=) #-}
{-# COMPILE GHC return = \_ -> return #-}

main : IO ⊤
main =
  putStrLn "Starting module exporter..." >>= λ _ →
  findAgdaFilesFFI "src/agda" >>= λ files →
  processAllAgdaFilesFFI files
