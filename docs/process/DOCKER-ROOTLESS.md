# Rootless Docker + GHCR Integration

## Overview

This repo supports building and pushing Docker images using rootless Docker and GitHub Container Registry (GHCR). Targets are generated from Agda and exposed via `make`.

## Prerequisites

- Rootless Docker configured (systemd user service). On Ubuntu/Kubuntu 25.10:
  - Install Docker rootless per official docs.
  - Ensure the rootful daemon is masked and user daemon active.
- GitHub PAT with `write:packages` scope.

## Environment Setup

Source the helper to export rootless env vars:

```bash
source scripts/docker-rootless-env.sh
```

Recommended defaults:

- `GHCR_REGISTRY=ghcr.io`
- `GHCR_USERNAME=<your_github_username>`

Set and persist (e.g., in `~/.bashrc`):

```bash
export GHCR_REGISTRY=ghcr.io
export GHCR_USERNAME="$USER_OR_GH"
```

Login to GHCR:

```bash
echo "$GITHUB_PAT" | docker login ghcr.io -u "$GHCR_USERNAME" --password-stdin
```

## Make Targets

- `make docker-rootless-status`: Show rootless status (`docker info`).
- `make docker-build`: Build local image `metacatagory:dev`.
- `make docker-build-ghcr`: Build and tag `ghcr.io/<user>/metacatagory:{dev,latest}`.
- `make docker-push-ghcr`: Push `dev` and `latest` tags to GHCR.
- `make docker-all`: Build + push pipeline.

## Quick Start

```bash
# Check daemon status
make docker-rootless-status

# Build local image
make docker-build

# Build & tag for GHCR
export GHCR_REGISTRY=ghcr.io
export GHCR_USERNAME=<your_github_username>
make docker-build-ghcr

# Push to GHCR (requires docker login)
make docker-push-ghcr
```

## Notes

- Makefile is generated from Agda (`make regen-makefile`). Do not edit Makefile directly.
- Docker targets live in `src/agda/Examples/Makefile/Targets/Docker.agda` and are imported by `Examples.ExporterMakefile`.
- The build image uses `Python 3.11-slim` and installs `requirements.txt`.
