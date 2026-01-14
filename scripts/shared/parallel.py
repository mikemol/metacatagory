#!/usr/bin/env python3
"""Shared parallelism helpers for scripts."""

from __future__ import annotations

from typing import Tuple


def get_parallel_settings() -> Tuple[bool, int]:
    """Return (parallel_enabled, worker_count) from shared config."""
    parallel = False
    workers = 1
    try:
        from .config import get_config

        config = get_config()
        parallel = config.parallel
        workers = max(1, int(config.workers))
    except Exception:
        parallel = False
        workers = 1
    return parallel, workers
