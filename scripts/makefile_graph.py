from __future__ import annotations

import collections
from typing import Dict, Iterable, Set


def parse_make_database(text: str) -> Dict[str, Set[str]]:
    graph: Dict[str, Set[str]] = collections.defaultdict(set)
    lines = text.splitlines()
    i = 0
    while i < len(lines):
        raw = lines[i].rstrip()
        if not raw or raw.startswith("#"):
            i += 1
            continue

        # Collect a multi-line rule (lines ending with '\').
        while raw.endswith("\\"):
            raw = raw[:-1].rstrip()
            i += 1
            if i >= len(lines):
                break
            raw += " " + lines[i].strip()

        if ":" in raw and not raw.startswith("\t"):
            target, rest = raw.split(":", 1)
            target = target.strip()
            deps = {dep for dep in rest.split() if dep}
            if target:
                graph[target].update(deps)
        i += 1
    return graph


def parse_phony_targets(graph: Dict[str, Set[str]]) -> Set[str]:
    phony: Set[str] = set()
    for target, deps in graph.items():
        if target == ".PHONY":
            phony.update(deps)
            break
    return phony


def reachable_nodes(root_targets: Iterable[str], graph: Dict[str, Set[str]]) -> Set[str]:
    visited: Set[str] = set()
    queue = collections.deque(root_targets)
    while queue:
        current = queue.popleft()
        if current in visited:
            continue
        visited.add(current)
        for dep in graph.get(current, set()):
            if dep not in visited:
                queue.append(dep)
    return visited
