#!/usr/bin/env sh
set -eu

sourced=0
(return 0 2>/dev/null) && sourced=1 || true
failure=0

if [ -n "${DOCKER_HOST:-}" ]; then
  case "$DOCKER_HOST" in
    unix://*)
      sock="${DOCKER_HOST#unix://}"
      if [ -S "$sock" ] && [ -w "$sock" ]; then
        failure=0
        if [ "$sourced" -eq 1 ]; then return 0; fi
        exit 0
      fi
      ;;
  esac
fi

if [ -z "${XDG_RUNTIME_DIR:-}" ]; then
  if command -v id >/dev/null 2>&1; then
    xdg_candidate="/run/user/$(id -u)"
    if [ -d "$xdg_candidate" ]; then
      XDG_RUNTIME_DIR="$xdg_candidate"
      export XDG_RUNTIME_DIR
    fi
  fi
fi

if [ -z "${XDG_RUNTIME_DIR:-}" ]; then
  echo "Rootless Docker: XDG_RUNTIME_DIR is unset and no /run/user/<uid> found." >&2
  failure=1
fi

if [ "$failure" -eq 0 ]; then
  candidates="$XDG_RUNTIME_DIR/docker.sock $XDG_RUNTIME_DIR/dockerd-rootless/api.sock"
  for sock in $candidates; do
    if [ -S "$sock" ]; then
      if [ -w "$sock" ]; then
        export DOCKER_HOST="unix://$sock"
        if [ -z "${DOCKER_CONTEXT:-}" ]; then
          export DOCKER_CONTEXT="rootless"
        fi
        if [ "$sourced" -eq 1 ]; then return 0; fi
        exit 0
      fi

      echo "Rootless Docker socket exists but is not writable: $sock" >&2
      if command -v stat >/dev/null 2>&1; then
        stat -c "socket perms: %a owner: %u gid: %g path: %n" "$sock" >&2 || true
      fi
      if command -v id >/dev/null 2>&1; then
        echo "Hint: start rootless dockerd with your primary gid mapped:" >&2
        echo "  dockerd-rootless.sh --group=$(id -g)" >&2
      fi
      failure=1
    fi
  done
fi

if [ "$failure" -eq 1 ]; then
  echo "No usable rootless Docker socket found under ${XDG_RUNTIME_DIR:-<unset>}." >&2
  echo "Start rootless dockerd (systemd user unit or dockerd-rootless.sh) and retry." >&2
  if [ "$sourced" -eq 1 ]; then return 1; fi
  exit 1
fi
