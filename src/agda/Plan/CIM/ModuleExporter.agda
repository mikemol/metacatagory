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
import           Data.Maybe (catMaybes)

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

-- Extract leading doc comment block (lines starting with -- or -- |)
extractDocBlock :: T.Text -> T.Text
extractDocBlock content =
  let ls = T.lines content
      dropWhileEnd p t = T.reverse (T.dropWhile p (T.reverse t))
      stripText t = T.dropWhile isSpace (dropWhileEnd isSpace t)
      cleaned = dropWhile (T.all isSpace) ls
      isDocLine l = let s = stripText l in T.isPrefixOf (T.pack "--") s
      docLines = takeWhile isDocLine cleaned
      stripDoc l =
        let s = stripText l
        in T.dropWhile isSpace (T.drop 2 s) -- drop leading "--"
  in T.unlines (map stripDoc docLines)

-- Extract docstrings attached to declarations (record/data/postulate).
extractDeclDocs :: [T.Text] -> [(T.Text, T.Text)]
extractDeclDocs ls = catMaybes (zipWith collect [0..] ls)
  where
    stripText t = T.dropWhile isSpace (dropWhileEnd isSpace t)
    dropWhileEnd p t = T.reverse (T.dropWhile p (T.reverse t))
    isDecl l = let s = stripText l in
      any (`T.isPrefixOf` s)
        [ T.pack "record "
        , T.pack "data "
        , T.pack "postulate "
        ]
    declName l =
      case T.words (stripText l) of
        (_ : name : _) -> name
        _              -> T.pack "(unknown)"
    isDocLine l =
      let s = stripText l in T.isPrefixOf (T.pack "--") s
    stripDoc l =
      let s = stripText l in T.dropWhile isSpace (T.drop 2 s)
    collect :: Int -> T.Text -> Maybe (T.Text, T.Text)
    collect idx l
      | not (isDecl l) = Nothing
      | otherwise =
          let prefix = take idx ls
              revDoc = takeWhile isDocLine (reverse prefix)
              docLines = reverse revDoc
              doc = T.unlines (map stripDoc docLines)
          in Just (declName l, doc)

-- Generate markdown with YAML frontmatter
generateModuleMarkdown :: T.Text -> [T.Text] -> T.Text -> [(T.Text, T.Text)] -> T.Text
generateModuleMarkdown modName imports docBlock declDocs =
  let frontmatter = T.unlines $
        [ T.pack "---"
        , T.pack "module: " `T.append` modName
        , T.pack "kind: per-module"
        , T.pack "imports:"
        ] ++ map (\imp -> T.pack "  - " `T.append` imp) imports ++
        [ T.pack "---" ]
      overview = if T.null (T.strip docBlock)
        then T.pack "*No overview provided.*\n"
        else docBlock `T.append` T.pack "\n"
      content = T.unlines
        [ T.pack ""
        , T.pack "# Module: " `T.append` modName
        , T.pack ""
        , T.pack "**Source:** `src/agda/" `T.append` T.replace (T.pack ".") (T.pack "/") modName `T.append` T.pack ".agda`"
        , T.pack ""
        , T.pack "## Overview"
        , T.pack ""
        , overview
        , T.pack "## Dependencies"
        , T.pack ""
        , T.pack "## Declarations"
        , T.pack ""
        ]
      importSection = if null imports
        then T.pack "*No imports*\n\n"
        else T.unlines $ map (\imp -> T.pack "* " `T.append` imp) imports
      declSection = if null declDocs
        then T.pack "*No documented declarations*\n\n"
        else T.unlines $
             map (\(n,d) ->
                    T.unlines
                      [ T.pack "* " `T.append` n
                      , if T.null (T.strip d)
                          then T.pack "  - (no doc)"
                          else T.pack "  - " `T.append` d
                      ]) declDocs
  in T.concat [frontmatter, content, importSection, declSection]

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
      docBlock = extractDocBlock content
      declDocs = extractDeclDocs (T.lines content)
      markdown = generateModuleMarkdown modName imports docBlock declDocs
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
