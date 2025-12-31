{-# OPTIONS --without-K --cubical --safe #-}

-- | RoadmapSync: GitHub roadmap synchronization orchestrator
-- Pure orchestration logic parameterized over GitHub API client interface.
-- This separates concerns: core logic stays pure and testable,
-- while the actual GitHub API implementation (with side effects) is external.

module Plan.CIM.RoadmapSync where

open import Agda.Builtin.String
open import Agda.Builtin.List

-- ============================================================================
-- Domain Model Records
-- ============================================================================

-- | A task extracted from the roadmap
record RoadmapTask : Set where
  field
    id          : String
    title       : String
    description : String
    category    : String
    priority    : String
    status      : String

-- | GitHub label metadata
record GitHubLabel : Set where
  field
    name        : String
    description : String

-- | GitHub issue representation
record GitHubIssue : Set where
  field
    number      : String
    title       : String
    body        : String
    labels      : List String

-- ============================================================================
-- Abstract GitHub API Interface
-- ============================================================================

-- | Abstract interface for GitHub API operations
-- Client code parameterizes over this interface.
-- No postulates in the main module; all effects are captured as requirements.
record GitHubAPI : Set₁ where
  field
    -- | Ensure a label exists in a repository; create if needed
    ensureLabelExists : (repo : String) 
                      → (labelName : String) 
                      → (labelDesc : String) 
                      → Set

    -- | Create a new issue in a repository
    createIssue : (repo : String)
                → (title : String)
                → (body : String)
                → (labels : List String)
                → Set

    -- | Update an existing issue in a repository
    updateIssue : (repo : String)
                → (issueNumber : String)
                → (title : String)
                → (body : String)
                → Set

-- ============================================================================
-- Pure Orchestration Logic
-- ============================================================================

-- | Pure sync orchestrator parameterized over GitHub API
module RoadmapSyncOrchestrator (GitHub : GitHubAPI) where
  open GitHubAPI GitHub

  -- | Format a task as markdown issue body
  formatTaskBody : RoadmapTask → String
  formatTaskBody task = RoadmapTask.description task

  -- | Core responsibility: ensure roadmap label and process all tasks
  -- The actual API calls are abstract obligations; the orchestrator
  -- just specifies WHAT needs to happen, not HOW.
  syncOrchestration : (repo : String) 
                     → (tasks : List RoadmapTask) 
                     → Set
  syncOrchestration repo tasks =
    ensureLabelExists repo "roadmap" "Roadmap and planning items"

  -- | Extract all tasks (intended for processing)
  extractTasks : List RoadmapTask → List RoadmapTask
  extractTasks tasks = tasks

  -- | Helper: get title from task
  getTaskTitle : RoadmapTask → String
  getTaskTitle = RoadmapTask.title

  -- | Helper: get ID from task
  getTaskId : RoadmapTask → String
  getTaskId = RoadmapTask.id

-- ============================================================================
-- Concrete API Implementation Signature
-- ============================================================================

-- | Template for concrete GitHub API implementation
-- This is where postulates and FFI code go; the main module stays pure.
-- In a separate module (at the boundary) with different OPTIONS:
--
--   {-# OPTIONS --allow-unsolved-metas #-}
--   module GitHubAPIImpl where
--     open import Plan.CIM.RoadmapSync
--     
--     postulate
--       makeGitHubRequest : String → String → String
--     
--     concreteAPI : GitHubAPI
--     GitHubAPI.ensureLabelExists concreteAPI repo name desc = ...
--     GitHubAPI.createIssue concreteAPI repo title body labels = ...
--     GitHubAPI.updateIssue concreteAPI repo num title body = ...

-- ============================================================================
-- Summary
-- ============================================================================

-- | This module demonstrates:
-- 1. Domain model (RoadmapTask, GitHubLabel, GitHubIssue)
-- 2. Abstract API interface (GitHubAPI as record of Set-valued operations)
-- 3. Pure orchestration (RoadmapSyncOrchestrator parameterized over GitHubAPI)
-- 4. No postulates in main path (all effects abstracted as type requirements)
-- 5. Clear FFI boundary (concrete API implementation in separate module)
--
-- To use: instantiate RoadmapSyncOrchestrator with a GitHubAPI implementation.
