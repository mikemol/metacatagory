#!/usr/bin/env python3
"""Validate JSON decomposition roundtrip: decompose → recompose → original (composition-based).

Showcases full integration of shared components:
- RecoveryPipeline for retry semantics
- ValidatedProvenance for lineage tracking
- StructuredLogger for progress tracking
- Validation for semantic comparison
"""

import json
import sys
import os
from pathlib import Path
from typing import Any, Dict, Tuple
from concurrent.futures import ThreadPoolExecutor

# Ensure repository root is importable as a package (scripts.*)
_REPO_ROOT = Path(__file__).resolve().parent.parent
if str(_REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(_REPO_ROOT))

from scripts.shared.parallel import get_parallel_settings
from scripts.shared.io import load_json
from scripts.shared.logging import configure_logging, StructuredLogger
from scripts.shared.validation import ValidationResult
from scripts.shared.validated_provenance import ValidatedProvenance
from scripts.shared.recovery_pipeline import RecoveryPipeline, RecoveryStrategy
from scripts.shared.pipelines import Phase


def get_all_values(data: Any) -> list[str]:
    """Recursively collect all leaf values from nested JSON-compatible structures."""

    values: list[str] = []

    if isinstance(data, dict):
        for value in data.values():
            values.extend(get_all_values(value))
    elif isinstance(data, (list, tuple)):
        for item in data:
            values.extend(get_all_values(item))
    else:
        values.append(str(data))

    return values


class LoadRoundtripFilesPhase(Phase[Path, Dict[str, Dict[str, Any]]]):
    """Phase: Load original and recomposed JSON files."""
    
    def __init__(self, recomposed_path: Path, logger: StructuredLogger):
        super().__init__("load_roundtrip", "Load roundtrip files")
        self.recomposed_path = recomposed_path
        self.logger = logger

    def transform(self, input_data: Path, context: Dict[str, Any]) -> Dict[str, Dict[str, Any]]:
        parallel, workers = get_parallel_settings()

        def load_file(path: Path) -> Dict[str, Any]:
            return load_json(path, required=True)

        if parallel and workers > 1:
            self.logger.info("Loading original file", file=str(input_data), mode="parallel")
            self.logger.info("Loading recomposed file", file=str(self.recomposed_path), mode="parallel")
            with ThreadPoolExecutor(max_workers=2) as executor:
                original_future = executor.submit(load_file, input_data)
                recomposed_future = executor.submit(load_file, self.recomposed_path)
                original = original_future.result()
                recomposed = recomposed_future.result()
        else:
            self.logger.info("Loading original file", file=str(input_data))
            original = load_json(input_data, required=True)
            
            self.logger.info("Loading recomposed file", file=str(self.recomposed_path))
            recomposed = load_json(self.recomposed_path, required=True)
        
        self.logger.info("Files loaded successfully")
        
        return {'original': original, 'recomposed': recomposed}


class ValidateSemanticEquivalencePhase(Phase[Dict[str, Dict[str, Any]], Dict[str, Any]]):
    """Phase: Validate semantic equivalence of original and recomposed."""
    
    def __init__(self, logger: StructuredLogger, provenance: ValidatedProvenance):
        super().__init__("validate_equivalence", "Validate semantic equivalence")
        self.logger = logger
        self.provenance = provenance

    def transform(self, input_data: Dict[str, Dict[str, Any]], 
                  context: Dict[str, Any]) -> Dict[str, Any]:
        original = input_data['original']
        recomposed = input_data['recomposed']
        
        # For decomposition strategy "dependency-graph", the structure changes:
        # Original: { "metadata": {...}, "nodes": [...] }
        # Recomposed: { "modules": [...], "edges": [...], "layers": [...] }
        # Both preserve the same number of modules (lossless at semantic level)
        
        original_modules = len(original.get("nodes", []))
        recomposed_modules = len(recomposed.get("modules", []))
        
        original_edges = len(original.get("edges", []))
        recomposed_edges = len(recomposed.get("edges", []))
        
        is_valid = (original_modules == recomposed_modules) and (original_edges == recomposed_edges)
        
        result = {
            'is_valid': is_valid,
            'original_modules': original_modules,
            'recomposed_modules': recomposed_modules,
            'original_edges': original_edges,
            'recomposed_edges': recomposed_edges,
            'modules_match': original_modules == recomposed_modules,
            'edges_match': original_edges == recomposed_edges
        }
        
        # Track validation in provenance
        self.provenance.add_validated_record(
            artifact_id="roundtrip_validation",
            record={
                'source_type': 'transformation',
                'source_id': 'roundtrip_comparison',
                'source_location': 'dependency_graph',
                'metadata': result
            }
        )
        
        self.logger.info(
            "Semantic validation complete",
            is_valid=is_valid,
            original_modules=original_modules,
            recomposed_modules=recomposed_modules
        )
        
        context['validation_result'] = result
        return result


class GenerateRoundtripReportPhase(Phase[Dict[str, Any], bool]):
    """Phase: Generate roundtrip validation report."""
    
    def __init__(self, original_path: Path, recomposed_path: Path, logger: StructuredLogger):
        super().__init__("generate_report", "Generate roundtrip report")
        self.original_path = original_path
        self.recomposed_path = recomposed_path
        self.logger = logger

    def transform(self, input_data: Dict[str, Any], context: Dict[str, Any]) -> bool:
        result = input_data
        
        if result['is_valid']:
            print("✅ JSON decomposition roundtrip PASSED (module count preserved)")
            print(f"   Original:   {self.original_path}")
            print(f"   Recomposed: {self.recomposed_path}")
            print(f"   Modules:    {result['original_modules']} ↔ {result['recomposed_modules']}")
            print(f"   Edges:      {result['original_edges']} ↔ {result['recomposed_edges']}")
            return True
        else:
            print("❌ JSON roundtrip validation FAILED (module/edge counts differ)")
            print(f"   Original:   {result['original_modules']} modules, {result['original_edges']} edges")
            print(f"   Recomposed: {result['recomposed_modules']} modules, {result['recomposed_edges']} edges")
            return False


def _strict_flag() -> bool:
    """Determine if strict structural comparison is enabled."""
    val = os.environ.get("METACATAGORY_STRICT_ROUNDTRIP")
    if val is None:
        # Default to strict
        return True
    return val.lower() in ("1", "true", "yes")


def validate_roundtrip(base_dir: Path | None = None) -> bool:
    """Validate decompose → recompose roundtrip (test-friendly)."""

    def log_context(orig: Path, reco: Path) -> None:
        print(f"[roundtrip] original={orig} recomposed={reco}")

    if base_dir is None:
        original_path = Path("build/dependency_graph.json")
        recomposed_path = Path("build/dependency_graph_recomposed.json")
        if not original_path.exists():
            legacy_original = Path("data/dependency_graph.json")
            if legacy_original.exists():
                original_path = legacy_original
                original_exists = True
            else:
                original_exists = False
        else:
            original_exists = True
    else:
        base_dir = Path(base_dir)
        original_path = base_dir / "dependency_graph.json"
        recomposed_path = base_dir / "dependency_graph_recomposed.json"
        original_exists = original_path.exists()

    log_context(original_path, recomposed_path)

    if not original_exists:
        print("Original file not found")
        return False
    recomposed_exists = recomposed_path.exists()
    if not recomposed_exists:
        print("Recomposed file not found")
        return False

    try:
        def load_file(path: Path) -> Dict[str, Any]:
            with open(path, "r", encoding="utf-8") as f:
                return json.load(f)

        parallel, workers = get_parallel_settings()
        if parallel and workers > 1:
            with ThreadPoolExecutor(max_workers=2) as executor:
                original_future = executor.submit(load_file, original_path)
                recomposed_future = executor.submit(load_file, recomposed_path)
                try:
                    original = original_future.result()
                except json.JSONDecodeError:
                    print("JSON parse error in original file")
                    return False
                try:
                    recomposed = recomposed_future.result()
                except json.JSONDecodeError:
                    print("JSON parse error in recomposed file")
                    return False
        else:
            original = load_file(original_path)
    except json.JSONDecodeError:
        print("JSON parse error in original file")
        return False

    if not (parallel and workers > 1):
        try:
            recomposed = load_file(recomposed_path)
        except json.JSONDecodeError:
            print("JSON parse error in recomposed file")
            return False

    original_modules, original_edges = _count_modules_edges(original)
    recomposed_modules, recomposed_edges = _count_modules_edges(recomposed)

    strict = _strict_flag()

    if strict:
        orig_mods, orig_edges_set = _collect_modules_and_edges(original)
        reco_mods, reco_edges_set = _collect_modules_and_edges(recomposed)
        if orig_mods == reco_mods and orig_edges_set == reco_edges_set:
            print("✅ JSON decomposition roundtrip PASSED (strict structural match)")
            print(f"   Original:   {original_path}")
            print(f"   Recomposed: {recomposed_path}")
            print(f"   Modules:    {len(orig_mods)} ↔ {len(reco_mods)}")
            print(f"   Edges:      {len(orig_edges_set)} ↔ {len(reco_edges_set)}")
            return True
        print("❌ JSON roundtrip validation FAILED (strict structural mismatch)")
        print(f"   Modules:    {len(orig_mods)} vs {len(reco_mods)} (set compare)")
        print(f"   Edges:      {len(orig_edges_set)} vs {len(reco_edges_set)} (set compare)")
        return False

    if original_modules == recomposed_modules and original_edges == recomposed_edges:
        print("✅ JSON decomposition roundtrip PASSED (module count preserved)")
        print(f"   Original:   {original_path}")
        print(f"   Recomposed: {recomposed_path}")
        print(f"   Modules:    {original_modules} ↔ {recomposed_modules}")
        print(f"   Edges:      {original_edges} ↔ {recomposed_edges}")
        return True

    print("❌ JSON roundtrip validation FAILED (module count differs)")
    print(f"   Original:   {original_modules} modules, {original_edges} edges ({original_path})")
    print(f"   Recomposed: {recomposed_modules} modules, {recomposed_edges} edges ({recomposed_path})")
    return False


def _count_modules_edges(data: Any) -> Tuple[int, int]:
    """Best-effort module/edge count across different schema shapes."""
    modules = 0
    edges = 0
    if isinstance(data, dict):
        if isinstance(data.get("nodes"), list):
            modules = len(data.get("nodes", []))
        elif "modules" in data:
            mods = data.get("modules")
            if isinstance(mods, list):
                modules = len(mods)
            elif isinstance(mods, dict):
                modules = len(mods)
        elif isinstance(data.get("items"), list):
            modules = len(data.get("items", []))
        elif isinstance(data.get("strategies"), list):
            modules = len(data.get("strategies", []))
        if isinstance(data.get("edges"), list):
            edges = len(data.get("edges", []))
    elif isinstance(data, list):
        modules = len(data)
    return modules, edges


def _collect_modules_and_edges(data: Any) -> Tuple[set[str], set[tuple[str, str]]]:
    """Collect module names and edge tuples for structural comparison."""
    modules: set[str] = set()
    edges: set[tuple[str, str]] = set()

    if isinstance(data, dict):
        if isinstance(data.get("nodes"), list):
            for node in data.get("nodes", []):
                name = node.get("module") or node.get("id")
                if isinstance(name, str):
                    modules.add(name)
                for imported in node.get("imports", []):
                    if isinstance(imported, str) and isinstance(name, str):
                        edges.add((name, imported))
        elif "modules" in data:
            mods = data.get("modules")
            if isinstance(mods, dict):
                modules.update(m for m in mods.keys() if isinstance(m, str))
            elif isinstance(mods, list):
                for m in mods:
                    if isinstance(m, dict):
                        name = m.get("name") or m.get("id")
                        if isinstance(name, str):
                            modules.add(name)
                    elif isinstance(m, str):
                        modules.add(m)
        if isinstance(data.get("edges"), list):
            for e in data.get("edges", []):
                if isinstance(e, dict):
                    a = e.get("from")
                    b = e.get("to")
                    if isinstance(a, str) and isinstance(b, str):
                        edges.add((a, b))
    elif isinstance(data, list):
        # Planning list schema: just count items, no edges
        modules.update(str(i) for i in range(len(data)))

    return modules, edges


def validate_roundtrip_with_paths(original_path: Path, recomposed_path: Path) -> bool:
    """Validate roundtrip given explicit paths (CLI helper)."""

    print(f"[roundtrip] original={original_path} recomposed={recomposed_path}")

    def load_file(path: Path) -> Dict[str, Any]:
        with open(path, "r", encoding="utf-8") as f:
            return json.load(f)

    try:
        original = load_file(original_path)
    except FileNotFoundError:
        print(f"Original file not found: {original_path}")
        return False
    except json.JSONDecodeError:
        print(f"JSON parse error in original file: {original_path}")
        return False

    try:
        recomposed = load_file(recomposed_path)
    except FileNotFoundError:
        print(f"Recomposed file not found: {recomposed_path}")
        return False
    except json.JSONDecodeError:
        print(f"JSON parse error in recomposed file: {recomposed_path}")
        return False

    original_modules, original_edges = _count_modules_edges(original)
    recomposed_modules, recomposed_edges = _count_modules_edges(recomposed)

    strict = _strict_flag()
    if strict:
        orig_mods, orig_edges_set = _collect_modules_and_edges(original)
        reco_mods, reco_edges_set = _collect_modules_and_edges(recomposed)
        if orig_mods == reco_mods and orig_edges_set == reco_edges_set:
            print("✅ JSON decomposition roundtrip PASSED (strict structural match)")
            print(f"   Original:   {original_path}")
            print(f"   Recomposed: {recomposed_path}")
            print(f"   Modules:    {len(orig_mods)} ↔ {len(reco_mods)}")
            print(f"   Edges:      {len(orig_edges_set)} ↔ {len(reco_edges_set)}")
            return True
        print("❌ JSON roundtrip validation FAILED (strict structural mismatch)")
        print(f"   Original:   {len(orig_mods)} modules, {len(orig_edges_set)} edges ({original_path})")
        print(f"   Recomposed: {len(reco_mods)} modules, {len(reco_edges_set)} edges ({recomposed_path})")
        return False

    if original_modules == recomposed_modules and original_edges == recomposed_edges:
        print("✅ JSON decomposition roundtrip PASSED (module count preserved)")
        print(f"   Original:   {original_path}")
        print(f"   Recomposed: {recomposed_path}")
        print(f"   Modules:    {original_modules} ↔ {recomposed_modules}")
        print(f"   Edges:      {original_edges} ↔ {recomposed_edges}")
        return True

    print("❌ JSON roundtrip validation FAILED (module count differs)")
    print(f"   Original:   {original_modules} modules, {original_edges} edges ({original_path})")
    print(f"   Recomposed: {recomposed_modules} modules, {recomposed_edges} edges ({recomposed_path})")
    return False


if __name__ == "__main__":
    args = sys.argv[1:]
    if len(args) == 2:
        original_arg = Path(args[0])
        recomposed_arg = Path(args[1])
        success = validate_roundtrip_with_paths(original_arg, recomposed_arg)
    else:
        success = validate_roundtrip()
    sys.exit(0 if success else 1)
