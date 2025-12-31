{-# OPTIONS --without-K #-}

-- | PlanningKernel: bridge module to expose the canonical roadmap as part of
-- a composite planning kernel.  This module imports the auto-generated
-- canonical items and lifts them into a ``RoadmapAdapter``.  By
-- constructing a list of adapters and passing it to ``unifiedIndex`` you
-- obtain a single consolidated roadmap.  Additional adapters may be
-- appended to the ``planningAdapters`` list without modifying the
-- canonical source.  Note: this file is part of the local branch and is
-- not auto-generated.

module Plan.CIM.PlanningKernel where

open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.String using (String)

open import Plan.CIM.RoadmapIndex using (RoadmapItem; RoadmapAdapter; unifiedIndex)
open import Plan.CIM.CanonicalRoadmap using (canonicalItems)

------------------------------------------------------------------------
-- Canonical adapter
------------------------------------------------------------------------

-- | ``canonicalAdapter`` lifts the list of ``canonicalItems`` into a
-- ``RoadmapAdapter``.  The name identifies the origin of these items.
canonicalAdapter : RoadmapAdapter
RoadmapAdapter.name canonicalAdapter = "canonical-roadmap"
RoadmapAdapter.items canonicalAdapter = canonicalItems

------------------------------------------------------------------------
-- Planning adapters and unified index
------------------------------------------------------------------------

-- | ``planningAdapters`` collects all adapters that participate in the
-- planning kernel.  To extend the kernel with your own tasks, define
-- another adapter and cons it onto this list.  For example:
-- ``myAdapter ∷ planningAdapters``.
planningAdapters : List RoadmapAdapter
planningAdapters = canonicalAdapter ∷ []

-- | ``planningIndex`` is the deduplicated list of ``RoadmapItem`` that
-- results from merging all adapters in ``planningAdapters``.  Use this
-- as the authoritative view of all tasks when performing introspective
-- planning.  See ``Plan.CIM.RoadmapIndex.unifiedIndex`` for details.
planningIndex : List RoadmapItem
planningIndex = unifiedIndex planningAdapters
