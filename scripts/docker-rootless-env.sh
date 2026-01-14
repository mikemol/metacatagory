#!/usr/bin/env sh
set -eu

script_dir=$(CDPATH= cd -- "$(dirname "$0")" && pwd)
. "$script_dir/ensure_rootless_docker.sh"

echo "Rootless Docker env set: ${DOCKER_HOST:-}"
