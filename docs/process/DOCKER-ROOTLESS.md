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

## ACT / Workflow Runs (Rootless)

When using `act` in this repo, use the rootless user socket and run through
`mise` (not the host shell). The working path that consistently succeeds here is
the user `docker.sock` (not the `dockerd-rootless/api.sock`, which returns
`404 page not found` for `docker info` and image queries).

```bash
# Force the correct rootless socket for act
export DOCKER_HOST="unix:///run/user/$(id -u)/docker.sock"

# List available workflows
MUTATE_LEVEL=repo mise exec -- make act-list

# Run CI workflow locally (requires container image pull)
MUTATE_LEVEL=repo mise exec -- make act-ci
```

Notes:
- If `act` reports `permission denied` to the socket, the rootless daemon needs
  to be started with a group mapping that includes your primary gid.
- Always use `MUTATE_LEVEL=repo` for `act-*` targets.
- Local `act` runs remap GitHub actions (checkout/upload/download) to a no-op
  action via `scripts/run_act.sh` to avoid network dependencies.

## Notes

- Makefile is generated from Agda (`make regen-makefile`). Do not edit Makefile directly.
- Docker targets live in `src/agda/Examples/Makefile/Targets/Docker.agda` and are imported by `Examples.ExporterMakefile`.
- The build image uses `Python 3.11-slim` and installs `requirements.txt`.
