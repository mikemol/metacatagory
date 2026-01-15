#!/usr/bin/env text
# Intake Directory Guide

This directory contains raw intake artifacts and working notes used to build
the formal roadmap. It is intentionally noisy. Use the guidance below to
identify the canonical sources.

## Canonical Sources

- `intake/GP/` is the authoritative set of GP source files for ingestion.
- `INTAKE-INGESTION-README.md` documents the ingestion pipeline.

## Non-Canonical / Archived Artifacts

- Top-level `GP*.md` files are historical snapshots retained for traceability.
- `__*.md` and `__*.md.backup*` files are conversational fragments and backups.
- Miscellaneous narrative drafts (long filenames) are archival context.

## Editing Guidance

- Prefer editing files under `intake/GP/` only when the intake source itself
  changes.
- For roadmap updates, edit the canonical Agda records or planning data and
  regenerate artifacts via the Makefile.
