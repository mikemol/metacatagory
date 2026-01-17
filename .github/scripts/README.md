#!/usr/bin/env text
# GitHub Automation Scripts

This directory contains helper scripts invoked by GitHub Actions workflows
for reporting, issue updates, and roadmap synchronization.

## Usage

These scripts are designed to be run through the CI workflow or Makefile
targets rather than manually. See `.github/workflows/ci.yml` and `Makefile`
for the entry points that call these scripts.

## Key Behaviors

- Report generation and issue tracking must respect the CI report/artifact
  directories configured in CI (`CI_REPORT_DIR`, `CI_ARTIFACT_DIR`).
- Scripts should be safe to run in both GitHub and `act` contexts.
