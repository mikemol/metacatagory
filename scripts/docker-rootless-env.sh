#!/usr/bin/env bash
# Helper to export rootless Docker environment variables.
export XDG_RUNTIME_DIR=/run/user/$UID
export DOCKER_HOST=unix:///run/user/$UID/docker.sock

echo "Rootless Docker env set: $DOCKER_HOST"