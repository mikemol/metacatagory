# Base worker image for act CI runs with Agda and pandoc preinstalled.
# Inherits from the standard act runner image (ubuntu 22.04).

FROM catthehacker/ubuntu:act-22.04

ENV DEBIAN_FRONTEND=noninteractive

# Core tooling for our workflows: Agda + stdlib + pandoc.
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        agda \
        agda-stdlib \
        pandoc \
        ghc \
        libghc-agda-dev \
        ca-certificates \
        git \
    && rm -rf /var/lib/apt/lists/*

# Configure Agda library defaults and record prim path for consistency with CI.
ENV AGDA_DIR=/usr/share/agda/lib \
    AGDA_STDLIB=/usr/share/agda-stdlib \
    AGDA_EXEC_OPTIONS="--include-path=/usr/share/agda/lib/prim"

# Resolve prim location and ensure /usr/share/agda/lib/prim exists (symlink when needed).
RUN set -e; \
    mkdir -p /root/.agda /usr/share/agda/lib; \
    echo "$AGDA_STDLIB/standard-library.agda-lib" > /root/.agda/libraries; \
    echo "standard-library" > /root/.agda/defaults; \
    PRIM=""; \
    for cand in /usr/share/agda/lib/prim /usr/lib/agda/lib/prim /usr/share/libghc-agda-dev/lib/prim; do \
      if [ -d "$cand" ]; then PRIM="$cand"; break; fi; \
    done; \
    if [ -z "$PRIM" ]; then echo "Agda prim not found" >&2; exit 1; fi; \
    if [ "$PRIM" != "/usr/share/agda/lib/prim" ]; then ln -s "$PRIM" /usr/share/agda/lib/prim; fi; \
    ls /usr/share/agda/lib/prim >/dev/null

# Keep image lean: no node/python preinstalls; actions/setup-* will fetch versions.
