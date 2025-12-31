{-# OPTIONS --without-K #-}

-- Tests/Chapters.agda
-- Connects the test harness to the Chapter 1–3 content by importing their indices.

module Tests.Chapters where

-- Importing these modules ensures all Chapter 1–3 content is typechecked
-- as part of the unified test suite.

import Chapter1.Level1Index
import Chapter2.Level2Index
import Chapter3.Level3Index
