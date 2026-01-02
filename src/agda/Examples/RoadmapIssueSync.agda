{-# OPTIONS --without-K #-}

-- | Example: sync roadmap items with external issue trackers.
module Examples.RoadmapIssueSync where

open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.String using (String; primStringAppend)
open import Agda.Builtin.Bool using (Bool; true; false)
-- Boolean if-then-else syntax support
if_then_else_ : Bool → {A : Set} → A → A → A
if true then x else y = x
if false then x else y = y

open import Agda.Builtin.Nat using (Nat)
open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.Unit using (⊤; tt)

{-# FOREIGN GHC
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS
import qualified Network.HTTP.Types.Status as HTTPStatus
import qualified System.Environment as Env
import           Control.Monad (forM_, when)
import           Data.Maybe (fromMaybe)

-- Task record from JSON
data Task = Task
  { taskId :: T.Text
  , taskTitle :: T.Text
  , taskStatus :: T.Text
  , taskSource :: T.Text
  , taskFiles :: [T.Text]
  , taskTags :: [T.Text]
  } deriving (Show)

instance Aeson.FromJSON Task where
  parseJSON = Aeson.withObject "Task" $ \v -> Task
    <$> v Aeson..: T.pack "id"
    <*> v Aeson..: T.pack "title"
    <*> v Aeson..: T.pack "status"
    <*> v Aeson..: T.pack "source"
    <*> v Aeson..: T.pack "files"
    <*> v Aeson..: T.pack "tags"

-- Issue record from GitHub API
data Issue = Issue
  { issueNumber :: Int
  , issueTitle :: T.Text
  } deriving (Show)

instance Aeson.FromJSON Issue where
  parseJSON = Aeson.withObject "Issue" $ \v -> Issue
    <$> v Aeson..: T.pack "number"
    <*> v Aeson..: T.pack "title"

-- Read tasks from JSON file
readTasksAdapter :: T.Text -> IO [Task]
readTasksAdapter path = do
  contents <- BSL.readFile (T.unpack path)
  case Aeson.decode contents of
    Nothing -> error $ "Failed to parse tasks file: " ++ T.unpack path
    Just tasks -> pure tasks

-- Get environment variable
getEnvAdapter :: T.Text -> IO (Maybe T.Text)
getEnvAdapter key = fmap T.pack <$> Env.lookupEnv (T.unpack key)

-- Create or update GitHub issue
createOrUpdateIssueAdapter :: T.Text -> T.Text -> T.Text -> Task -> Maybe Int -> IO ()
createOrUpdateIssueAdapter token repo apiRoot task mNumber = do
  manager <- HTTPS.newTlsManager
  let issueTitle = T.concat [T.pack "[Roadmap] ", taskId task, T.pack " - ", taskTitle task]
      body = formatBody task
      url = case mNumber of
        Nothing -> T.concat [apiRoot, T.pack "/repos/", repo, T.pack "/issues"]
        Just n -> T.concat [apiRoot, T.pack "/repos/", repo, T.pack "/issues/", T.pack (show n)]
      method = case mNumber of
        Nothing -> "POST"
        Just _ -> "PATCH"
      
      payload = Aeson.object
        [ T.pack "title" Aeson..= issueTitle
        , T.pack "body" Aeson..= body
        , T.pack "labels" Aeson..= ([T.pack "roadmap"] :: [T.Text])
        ]
  
  initReq <- HTTP.parseRequest (T.unpack url)
  let req = initReq
        { HTTP.method = method
        , HTTP.requestHeaders =
          [ (T.pack "Authorization", T.encodeUtf8 $ T.concat [T.pack "Bearer ", token])
          , (T.pack "Accept", T.pack "application/vnd.github+json")
          , (T.pack "Content-Type", T.pack "application/json")
          , (T.pack "User-Agent", T.pack "metacatagory-roadmap-sync")
          ]
        , HTTP.requestBody = HTTP.RequestBodyLBS (Aeson.encode payload)
        }
  
  response <- HTTP.httpLbs req manager
  let status = HTTP.responseStatus response
  when (HTTPStatus.statusCode status >= 300) $
    error $ "HTTP request failed with status: " ++ show status
  
  case mNumber of
    Nothing -> putStrLn $ "Created issue: " ++ T.unpack issueTitle
    Just n -> putStrLn $ "Updated issue #" ++ show n ++ ": " ++ T.unpack issueTitle
  where
    formatBody :: Task -> T.Text
    formatBody t = T.concat
      [ T.pack "### Roadmap Task\\n\\n"
      , T.pack "ID: ", taskId t, T.pack "\\n"
      , T.pack "Status: ", taskStatus t, T.pack "\\n"
      , T.pack "Source: ", taskSource t, T.pack "\\n\\n"
      , T.pack "Files: ", T.intercalate (T.pack ", ") (taskFiles t), T.pack "\\n"
      , T.pack "Tags: ", T.intercalate (T.pack ", ") (taskTags t), T.pack "\\n\\n"
      , T.pack "This issue was auto-generated. Update status by editing tasks.json and re-running sync."
      ]

import qualified Data.Text.Encoding as TE

-- Fetch existing roadmap issues
fetchExistingIssuesAdapter :: T.Text -> T.Text -> T.Text -> IO [Issue]
fetchExistingIssuesAdapter token repo apiRoot = do
  manager <- HTTPS.newTlsManager
  let url = T.concat [apiRoot, T.pack "/repos/", repo, T.pack "/issues?state=open&labels=roadmap"]
  
  initReq <- HTTP.parseRequest (T.unpack url)
  let req = initReq
        { HTTP.requestHeaders =
          [ (T.pack "Authorization", TE.encodeUtf8 $ T.concat [T.pack "Bearer ", token])
          , (T.pack "Accept", T.pack "application/vnd.github+json")
          , (T.pack "User-Agent", T.pack "metacatagory-roadmap-sync")
          ]
        }
  
  response <- HTTP.httpLbs req manager
  case Aeson.decode (HTTP.responseBody response) of
    Nothing -> error "Failed to parse existing issues"
    Just issues -> pure issues

-- Ensure roadmap label exists
ensureRoadmapLabelAdapter :: T.Text -> T.Text -> T.Text -> IO ()
ensureRoadmapLabelAdapter token repo apiRoot = do
  manager <- HTTPS.newTlsManager
  let url = T.concat [apiRoot, T.pack "/repos/", repo, T.pack "/labels"]
      payload = Aeson.object
        [ T.pack "name" Aeson..= T.pack "roadmap"
        , T.pack "color" Aeson..= T.pack "0e8a16"
        , T.pack "description" Aeson..= T.pack "MetaCategory roadmap task"
        ]
  
  initReq <- HTTP.parseRequest (T.unpack url)
  let req = initReq
        { HTTP.method = "POST"
        , HTTP.requestHeaders =
          [ (T.pack "Authorization", TE.encodeUtf8 $ T.concat [T.pack "Bearer ", token])
          , (T.pack "Accept", T.pack "application/vnd.github+json")
          , (T.pack "Content-Type", T.pack "application/json")
          , (T.pack "User-Agent", T.pack "metacatagory-roadmap-sync")
          ]
        , HTTP.requestBody = HTTP.RequestBodyLBS (Aeson.encode payload)
        }
  
  _ <- HTTP.httpLbs req manager  -- Ignore errors if label already exists
  putStrLn "Ensured roadmap label exists"
#-}

-- Core data types (declare before FFI usage)
postulate
  IssueNumber : Set
  issueNumberToNat : IssueNumber → Nat

{-# COMPILE GHC IssueNumber = type Int #-}
{-# COMPILE GHC issueNumberToNat = toInteger #-}

record GitHubIssue : Set where
  -- | Minimal GitHub issue payload exposed to Agda/FFI.
  field
    number : IssueNumber
    title : String

{-# COMPILE GHC GitHubIssue = data Issue (Issue) #-}

record RoadmapTask : Set where
  -- | Subset of roadmap task fields used for syncing.
  field
    id : String
    title : String
    status : String
    source : String
    files : List String
    tags : List String

{-# COMPILE GHC RoadmapTask = data Task (Task) #-}

-- Maybe type (needed before FFI signatures using Maybe)
-- | Option type exposed for FFI interop.
data Maybe (A : Set) : Set where
  nothing : Maybe A
  just : A → Maybe A

{-# COMPILE GHC Maybe = data Maybe (Nothing | Just) #-}

-- Postulated FFI functions (after types are in scope)
postulate
  -- | Read roadmap tasks from JSON file.
  readTasks : String → IO (List RoadmapTask)
  -- | Look up an environment variable.
  getEnv : String → IO (Maybe String)
  -- | Ensure a GitHub label exists.
  ensureRoadmapLabel : String → String → String → IO ⊤
  -- | Fetch issues from GitHub matching roadmap labels.
  fetchExistingIssues : String → String → String → IO (List GitHubIssue)
  -- | Create or update a GitHub issue corresponding to a roadmap task.
  createOrUpdateIssue : String → String → String → RoadmapTask → Maybe IssueNumber → IO ⊤

{-# COMPILE GHC readTasks = readTasksAdapter #-}
{-# COMPILE GHC getEnv = getEnvAdapter #-}
{-# COMPILE GHC ensureRoadmapLabel = ensureRoadmapLabelAdapter #-}
{-# COMPILE GHC fetchExistingIssues = fetchExistingIssuesAdapter #-}
{-# COMPILE GHC createOrUpdateIssue = createOrUpdateIssueAdapter #-}

-- String helpers
_++_ : String → String → String
_++_ = primStringAppend

-- String equality (postulated)
postulate
  stringEquals : String → String → Bool

{-# FOREIGN GHC
stringEqualsAdapter :: T.Text -> T.Text -> Bool
stringEqualsAdapter = (==)
#-}

{-# COMPILE GHC stringEquals = stringEqualsAdapter #-}

-- IO monad operations
postulate
  _>>=_ : {A B : Set} → IO A → (A → IO B) → IO B
  return : {A : Set} → A → IO A
  putStrLn : String → IO ⊤
  _>>_ : {A B : Set} → IO A → IO B → IO B

{-# COMPILE GHC _>>=_ = \_ _ -> (>>=) #-}
{-# COMPILE GHC return = \_ -> pure #-}
{-# COMPILE GHC putStrLn = putStrLn . T.unpack #-}
{-# COMPILE GHC _>>_ = \_ _ -> (>>) #-}

-- Find existing issue by title
findIssueByTitle : String → List GitHubIssue → Maybe IssueNumber
findIssueByTitle targetTitle [] = nothing
findIssueByTitle targetTitle (issue ∷ rest) =
  if stringEquals (GitHubIssue.title issue) targetTitle
  then just (GitHubIssue.number issue)
  else findIssueByTitle targetTitle rest

-- Build issue title from task
buildIssueTitle : RoadmapTask → String
buildIssueTitle task = ("[Roadmap] " ++ RoadmapTask.id task) ++ (" - " ++ RoadmapTask.title task)

-- Process single task
processTask : String → String → String → List GitHubIssue → RoadmapTask → IO ⊤
processTask token repo apiRoot existingIssues task = do
  let issueTitle = buildIssueTitle task
  let mIssueNumber = findIssueByTitle issueTitle existingIssues
  createOrUpdateIssue token repo apiRoot task mIssueNumber
  return tt

-- Process all tasks
processTasks : String → String → String → List GitHubIssue → List RoadmapTask → IO ⊤
processTasks token repo apiRoot existingIssues [] = return tt
processTasks token repo apiRoot existingIssues (task ∷ tasks) = do
  processTask token repo apiRoot existingIssues task
  processTasks token repo apiRoot existingIssues tasks

-- Validation: check required environment variables
validateEnvironment : Maybe String → Maybe String → Bool
validateEnvironment nothing _ = false
validateEnvironment _ nothing = false
validateEnvironment (just _) (just _) = true

-- Main sync function
runRoadmapIssueSync : String → IO ⊤
runRoadmapIssueSync tasksFile = do
  putStrLn "Starting roadmap issue synchronization"
  mToken ← getEnv "GITHUB_TOKEN"
  mRepo ← getEnv "GITHUB_REPOSITORY"
  handleEnv mToken mRepo
  where
    handleEnv : Maybe String → Maybe String → IO ⊤
    handleEnv nothing _ = do
      putStrLn "ERROR: GITHUB_TOKEN not set"
      return tt
    handleEnv (just _) nothing = do
      putStrLn "ERROR: GITHUB_REPOSITORY not set (expected owner/repo)"
      return tt
    handleEnv (just token) (just repo) = do
      let apiRoot = "https://api.github.com"
      putStrLn "Reading tasks file"
      tasks ← readTasks tasksFile
      putStrLn "Ensuring roadmap label exists"
      ensureRoadmapLabel token repo apiRoot
      putStrLn "Fetching existing roadmap issues"
      existingIssues ← fetchExistingIssues token repo apiRoot
      putStrLn "Processing tasks"
      processTasks token repo apiRoot existingIssues tasks
      putStrLn "Roadmap sync complete"
      return tt

-- Default entry point
main : IO ⊤
main = runRoadmapIssueSync ".github/roadmap/tasks.json"

-- Compile to executable
{-# FOREIGN GHC
main :: IO ()
main = runRoadmapIssueSync (T.pack ".github/roadmap/tasks.json")
#-}
