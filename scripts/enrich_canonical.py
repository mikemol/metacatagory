#!/usr/bin/env python3
"""
Enrich canonical roadmap with semantic details extracted from source materials.

Reads data/planning_index.json and outputs build/canonical_enriched.json
with additional fields:
  - intent: why the task exists (1-2 sentences)
  - deliverable: concrete artifact/behavior expected
  - scope: {in: [...], out: [...]} inclusions/exclusions
  - inputs: source artifacts consumed
  - outputs: artifacts produced
  - acceptance: checklist for completion
  - risks: known blockers or complexity notes
  - complexity: estimated effort (low/medium/high)
  - evidence: [{source, loc, text}] snippets from provenance
  - derivedTags: normalized tag vocabulary
  - moduleAnchors: Agda module names
  - docAnchors: markdown heading paths
"""

import json
import re
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
from typing import List, Dict, Any, Optional

# --- Configuration --------------------------------------------------------

REPO_ROOT = Path(__file__).parent.parent
CANONICAL_JSON = REPO_ROOT / "data" / "planning_index.json"
ENRICHED_JSON = REPO_ROOT / "build" / "canonical_enriched.json"

from shared.parallel import get_parallel_settings

# Controlled tag vocabulary
TAG_VOCAB = {
    "hott": ["HoTT", "Path", "Univalence"],
    "adjunction": ["Adjunction", "Free", "Forgetful"],
    "yoneda": ["Yoneda", "Presheaf"],
    "fibration": ["Fibration", "BeckChevalley", "Grothendieck"],
    "limits": ["Limit", "Colimit", "Cone", "Cocone"],
    "universal": ["UniversalProperty", "Initial", "Terminal"],
    "serialization": ["Serialization", "Roundtrip"],
    "topos": ["Topos", "Subobject"],
    "indexing": ["Indexing", "Coordinate", "WellFounded"],
    "ingestion": ["Ingestion", "Import"],
    "errors": ["ErrorHandling", "Limitation", "Specification"],
    "metrics": ["Metrics", "Growth", "Performance"],
    "algebra": ["Groups", "Rings", "Fields", "Ideals", "Cyclic"],
    "testing": ["Testing", "Validation", "Checklist"],
    "phase": ["Phase", "PhaseI", "PhaseII", "PhaseIII", "PhaseIV", "PhaseV"]
}

# --- Markdown extraction --------------------------------------------------

def extract_markdown_section(md_path: Path, heading_pattern: str) -> Optional[Dict[str, str]]:
    """
    Find heading matching pattern and extract following content until next heading.
    Returns {heading, text, line_start}.
    """
    if not md_path.exists():
        return None
    
    with open(md_path, "r", encoding="utf-8") as f:
        lines = f.readlines()
    
    heading = None
    heading_line = 0
    content_lines = []
    in_section = False
    
    for i, line in enumerate(lines, start=1):
        # Check for heading match
        if re.match(r"^#+\s+", line):
            if heading_pattern.lower() in line.lower():
                heading = line.strip()
                heading_line = i
                in_section = True
                continue
            elif in_section:
                # Hit next heading at same or higher level, stop
                break
        
        # Collect content lines after heading
        if in_section:
            stripped = line.strip()
            if stripped and not stripped.startswith("```"):
                content_lines.append(stripped)
            # Allow multiple paragraphs separated by blank lines
    
    if heading and content_lines:
        # Join lines, preserving paragraph breaks
        text = " ".join(content_lines)
        return {
            "heading": heading,
            "text": text,
            "line_start": heading_line
        }
    return None

def extract_evidence_from_markdown(item: Dict) -> List[Dict[str, str]]:
    """
    Extract evidence snippets from markdown sources in provenance.
    """
    evidence = []
    title = item.get("title", "")
    
    for prov in item.get("provenance", []):
        parts = prov.split("|")
        if len(parts) < 2:
            continue
        
        source = parts[1]
        if not source.endswith(".md"):
            continue
        
        md_path = REPO_ROOT / source
        section = extract_markdown_section(md_path, title)
        
        if section:
            evidence.append({
                "source": source,
                "loc": f"line {section['line_start']}",
                "text": section["text"][:200] + ("..." if len(section["text"]) > 200 else "")
            })
    
    return evidence

# --- Agda extraction ------------------------------------------------------

def extract_agda_module_header(agda_path: Path) -> Optional[str]:
    """
    Extract module doc comment or first meaningful block comment.
    """
    if not agda_path.exists():
        return None
    
    with open(agda_path, "r", encoding="utf-8") as f:
        lines = f.readlines()
    
    doc_lines = []
    in_block_comment = False
    
    for line in lines[:50]:  # Check first 50 lines
        stripped = line.strip()
        
        # Skip OPTIONS pragmas
        if stripped.startswith("{-# OPTIONS"):
            continue
        
        # Handle block comments {- ... -}
        if "{-" in stripped and "-}" not in stripped:
            in_block_comment = True
            # Extract text after {-
            text = stripped.split("{-", 1)[1].strip()
            if text and not text.startswith("#"):
                doc_lines.append(text)
            continue
        
        if in_block_comment:
            if "-}" in stripped:
                in_block_comment = False
                # Extract text before -}
                text = stripped.split("-}", 1)[0].strip()
                if text:
                    doc_lines.append(text)
                break
            else:
                if stripped and not stripped.startswith("-"):
                    doc_lines.append(stripped)
        
        # Handle single-line comments starting with --
        if stripped.startswith("--") and not stripped.startswith("---"):
            text = stripped[2:].strip()
            if text and len(text) > 10:  # Meaningful comments
                doc_lines.append(text)
                if len(doc_lines) >= 3:
                    break
    
    if doc_lines:
        return " ".join(doc_lines[:5])  # Return up to 5 lines
    
    return None

def extract_agda_exports(agda_path: Path) -> List[str]:
    """
    Extract top-level definition names (heuristic: lines starting with name :).
    """
    if not agda_path.exists():
        return []
    
    with open(agda_path, "r", encoding="utf-8") as f:
        lines = f.readlines()
    
    exports = []
    for line in lines:
        # Match: identifier : type
        match = re.match(r"^([a-zA-Z][a-zA-Z0-9-_]*)\s*:", line)
        if match:
            exports.append(match.group(1))
    
    return exports[:5]  # Limit to first 5

def parse_dot_dependency_graph(dot_path: Path) -> Dict[str, List[str]]:
    """
    Parse Agda --dependency-graph DOT file.
    Returns dict mapping module name to list of modules it depends on.
    """
    if not dot_path.exists():
        return {}
    
    with open(dot_path, "r", encoding="utf-8") as f:
        content = f.read()
    
    # Build node ID to module name mapping
    node_labels = {}
    for match in re.finditer(r'm(\d+)\[label="([^"]+)"\]', content):
        node_id = f"m{match.group(1)}"
        module_name = match.group(2)
        node_labels[node_id] = module_name
    
    # Build dependency graph (edges)
    dependencies: Dict[str, List[str]] = {}
    for match in re.finditer(r'm(\d+)\s*->\s*m(\d+)', content):
        src_id = f"m{match.group(1)}"
        dst_id = f"m{match.group(2)}"
        
        src_module = node_labels.get(src_id)
        dst_module = node_labels.get(dst_id)
        
        if src_module and dst_module:
            # Filter out Agda builtins/stdlib
            if not dst_module.startswith(("Agda.", "Data.", "Relation.", "Function.")):
                dependencies.setdefault(src_module, []).append(dst_module)
    
    return dependencies

def build_module_to_tasks_map(items: List[Dict]) -> Dict[str, List[str]]:
    """
    Build a mapping from module names to task IDs that reference them.
    """
    module_to_tasks: Dict[str, List[str]] = {}

    parallel, workers = get_parallel_settings()

    def extract_pairs(item: Dict[str, Any]) -> List[tuple[str, str]]:
        pairs: List[tuple[str, str]] = []
        task_id = item.get("id", "")
        for file_path in item.get("files", []):
            if not file_path.endswith(".agda"):
                continue

            # Convert path to module name
            parts = Path(file_path).parts
            if "agda" in parts:
                idx = parts.index("agda")
                module_parts = parts[idx + 1:]
                module_name = ".".join(module_parts).replace(".agda", "")
                pairs.append((module_name, task_id))
        return pairs

    if parallel and workers > 1 and items:
        with ThreadPoolExecutor(max_workers=workers) as executor:
            results = list(executor.map(extract_pairs, items))
    else:
        results = [extract_pairs(item) for item in items]

    for pairs in results:
        for module_name, task_id in pairs:
            module_to_tasks.setdefault(module_name, []).append(task_id)
    
    return module_to_tasks

def extract_evidence_from_agda(item: Dict) -> List[Dict[str, str]]:
    """
    Extract evidence from Agda files in the files list.
    """
    evidence = []
    
    for file_path in item.get("files", []):
        if not file_path.endswith(".agda"):
            continue
        
        agda_path = REPO_ROOT / file_path
        doc = extract_agda_module_header(agda_path)
        
        if doc:
            evidence.append({
                "source": file_path,
                "loc": "module header",
                "text": doc[:200] + ("..." if len(doc) > 200 else "")
            })
    
    return evidence

def extract_module_anchors(item: Dict) -> List[str]:
    """
    Extract Agda module names from files.
    """
    anchors = []
    
    for file_path in item.get("files", []):
        if not file_path.endswith(".agda"):
            continue
        
        # Convert path to module name: src/agda/Core/Yoneda.agda -> Core.Yoneda
        parts = Path(file_path).parts
        if "agda" in parts:
            idx = parts.index("agda")
            module_parts = parts[idx + 1:]
            module_name = ".".join(module_parts).replace(".agda", "")
            anchors.append(module_name)
    
    return anchors

# --- Tag normalization ----------------------------------------------------

def normalize_tags(item: Dict) -> List[str]:
    """
    Map existing tags to controlled vocabulary.
    """
    existing = [t.lower() for t in item.get("tags", [])]
    derived = set()
    
    for category, vocab in TAG_VOCAB.items():
        for tag in vocab:
            if tag.lower() in existing or category in existing:
                derived.update(vocab)
                break
    
    # Add original tags not in vocab
    for tag in item.get("tags", []):
        if tag not in derived:
            derived.add(tag)
    
    return sorted(derived)

# --- Semantic extraction --------------------------------------------------

def infer_intent(item: Dict, evidence: List[Dict]) -> str:
    """
    Infer intent from title and evidence.
    """
    title = item.get("title", "")
    
    # Use Agda evidence first (more structured)
    agda_evidence = [e for e in evidence if e["source"].endswith(".agda")]
    md_evidence = [e for e in evidence if e["source"].endswith(".md")]
    
    if agda_evidence:
        text = agda_evidence[0]["text"]
        # Clean up and extract meaningful intent
        text = re.sub(r"#\s*OPTIONS.*?", "", text).strip()
        if len(text) > 20:
            return text[:250] + ("..." if len(text) > 250 else "")
    
    if md_evidence:
        text = md_evidence[0]["text"]
        if len(text) > 20:
            return text[:250] + ("..." if len(text) > 250 else "")
    
    # Fallback heuristics
    if title.startswith("Ingest"):
        return f"Formalize and integrate {title.replace('Ingest', '').strip()} into the algebra hierarchy."
    
    if "postulate" in title.lower():
        return "Replace non-constructive postulate with proven implementation."
    
    if "test" in title.lower() or "checklist" in title.lower():
        return f"Validate {title} with executable tests and comprehensive coverage."
    
    if "yoneda" in title.lower():
        return "Formalize the Yoneda lemma and embedding with constructive proofs, establishing the bijection between natural transformations and functor evaluations."
    
    if "adjunction" in title.lower():
        return "Establish adjunction coherence properties, proving the free/forgetful functor relationships with unit and counit natural transformations."
    
    return f"Implement {title} as specified."

def infer_deliverable(item: Dict) -> str:
    """
    Infer concrete deliverable from files and title.
    """
    files = item.get("files", [])
    title = item.get("title", "")
    
    agda_files = [f for f in files if f.endswith(".agda")]
    test_files = [f for f in agda_files if "Test" in f]
    
    if test_files:
        return f"Passing tests in {', '.join(Path(f).name for f in test_files[:2])}"
    
    if agda_files:
        return f"Compiled Agda modules: {', '.join(Path(f).name for f in agda_files[:2])}"
    
    return title

def infer_acceptance(item: Dict, definitions: List[str]) -> List[str]:
    """
    Generate acceptance checklist based on status, files, and extracted definitions.
    """
    status = item.get("status", "not-started")
    files = item.get("files", [])
    
    acceptance = []
    
    agda_files = [f for f in files if f.endswith(".agda")]
    test_files = [f for f in agda_files if "Test" in f]
    
    if agda_files:
        acceptance.append(f"All Agda modules compile without errors")
    
    if test_files:
        acceptance.append(f"Test suite passes: {', '.join(Path(f).name for f in test_files)}")
    
    # Add definition-specific criteria
    if definitions:
        def_list = ", ".join(definitions[:3])
        if len(definitions) > 3:
            def_list += f" (+ {len(definitions) - 3} more)"
        acceptance.append(f"Key definitions implemented: {def_list}")
    
    if status == "completed":
        acceptance.append("Implementation verified in canonical roadmap")
    else:
        acceptance.append("Code review and integration approval")
    
    return acceptance

def infer_scope(item: Dict) -> Dict[str, List[str]]:
    """
    Infer scope boundaries from title and tags.
    """
    title = item.get("title", "").lower()
    tags = [t.lower() for t in item.get("tags", [])]
    
    scope_in = []
    scope_out = []
    
    # Heuristics
    if "constructive" in title:
        scope_in.append("Constructive proofs")
        scope_out.append("Postulated axioms")
    
    if "hott" in tags or "path" in tags:
        scope_in.append("HoTT path equality")
        scope_out.append("Propositional equality only")
    
    if "test" in title:
        scope_in.append("Executable test coverage")
        scope_out.append("Informal validation")
    
    return {"in": scope_in, "out": scope_out}

def infer_inputs_outputs(item: Dict) -> Dict[str, List[str]]:
    """
    Infer inputs/outputs from files and dependencies.
    """
    files = item.get("files", [])
    depends = item.get("dependsOn", [])
    
    inputs = []
    outputs = []
    
    if depends:
        inputs.append(f"Dependencies: {', '.join(depends[:3])}")
    
    agda_files = [f for f in files if f.endswith(".agda")]
    if agda_files:
        outputs.append(f"Agda modules: {', '.join(Path(f).name for f in agda_files[:3])}")
    
    return {"inputs": inputs, "outputs": outputs}

def infer_dependencies_from_imports(item: Dict, module_to_tasks: Dict[str, List[str]], dep_graph: Dict[str, List[str]]) -> List[str]:
    """
    Suggest additional dependencies based on Agda dependency graph.
    Returns list of task IDs that this task might depend on.
    """
    suggested_deps = set()
    current_task_id = item.get("id", "")
    
    for file_path in item.get("files", []):
        if not file_path.endswith(".agda"):
            continue
        
        # Convert path to module name
        parts = Path(file_path).parts
        if "agda" in parts:
            idx = parts.index("agda")
            module_parts = parts[idx + 1:]
            module_name = ".".join(module_parts).replace(".agda", "")
            
            # Get dependencies from DOT graph
            imported_modules = dep_graph.get(module_name, [])
            
            # Find tasks that define those imported modules
            for imported_module in imported_modules:
                related_tasks = module_to_tasks.get(imported_module, [])
                for task_id in related_tasks:
                    if task_id != current_task_id:
                        suggested_deps.add(task_id)
    
    return sorted(suggested_deps)

def estimate_complexity(item: Dict) -> str:
    """
    Estimate complexity from files, dependencies, and tags.
    """
    files = item.get("files", [])
    depends = item.get("dependsOn", [])
    tags = item.get("tags", [])
    
    score = 0
    
    score += len(files)
    score += len(depends) * 2
    
    if any(t in ["HoTT", "Fibration", "Yoneda", "Adjunction"] for t in tags):
        score += 3
    
    if score <= 2:
        return "low"
    elif score <= 5:
        return "medium"
    else:
        return "high"

# --- Main enrichment pipeline ---------------------------------------------

def enrich_item(item: Dict, module_to_tasks: Dict[str, List[str]], dep_graph: Dict[str, List[str]]) -> Dict:
    """
    Enrich a single roadmap item with semantic details.
    """
    # Extract evidence
    md_evidence = extract_evidence_from_markdown(item)
    agda_evidence = extract_evidence_from_agda(item)
    all_evidence = md_evidence + agda_evidence
    
    # Extract definitions from Agda files
    all_definitions = []
    for file_path in item.get("files", []):
        if file_path.endswith(".agda"):
            agda_path = REPO_ROOT / file_path
            defs = extract_agda_exports(agda_path)
            all_definitions.extend(defs)
    
    # Infer dependencies from Agda dependency graph
    suggested_deps = infer_dependencies_from_imports(item, module_to_tasks, dep_graph)
    
    # Derive semantic fields
    enriched = dict(item)  # Copy original
    
    enriched["intent"] = infer_intent(item, all_evidence)
    enriched["deliverable"] = infer_deliverable(item)
    enriched["scope"] = infer_scope(item)
    enriched["acceptance"] = infer_acceptance(item, all_definitions)
    enriched["evidence"] = all_evidence
    enriched["derivedTags"] = normalize_tags(item)
    enriched["moduleAnchors"] = extract_module_anchors(item)
    enriched["complexity"] = estimate_complexity(item)
    
    io = infer_inputs_outputs(item)
    enriched["inputs"] = io["inputs"]
    enriched["outputs"] = io["outputs"]
    
    # Add suggested dependencies (merge with existing)
    enriched["suggestedDependencies"] = suggested_deps
    
    # Add extracted definitions
    enriched["definitions"] = all_definitions[:10]  # Limit to 10
    
    # Optional: risks (placeholder)
    enriched["risks"] = []
    
    return enriched

def enrich_canonical() -> None:
    """
    Main entry point: read canonical, enrich, write enriched.
    """
    if not CANONICAL_JSON.exists():
        print(f"Error: {CANONICAL_JSON} not found. Run 'make roadmap-merge' first.")
        return
    
    with open(CANONICAL_JSON, "r", encoding="utf-8") as f:
        canonical = json.load(f)
    
    print(f"Enriching {len(canonical)} items...")
    
    # Load Agda dependency graph from DOT file
    dot_path = REPO_ROOT / "build" / "diagrams" / "agda-deps-full.dot"
    if not dot_path.exists():
        # Fallback to smaller graph
        dot_path = REPO_ROOT / "build" / "diagrams" / "agda-deps.dot"
    
    if dot_path.exists():
        print(f"Loading Agda dependency graph from {dot_path.name}...")
        dep_graph = parse_dot_dependency_graph(dot_path)
        print(f"  Found {len(dep_graph)} modules with {sum(len(v) for v in dep_graph.values())} edges")
    else:
        print(f"Warning: Agda dependency graph not found. Dependency suggestions will be limited.")
        print("  Run: agda --dependency-graph=build/diagrams/agda-deps-full.dot -i src/agda src/agda/Tests/Index.agda")
        dep_graph = {}
    
    # Build module-to-tasks mapping for dependency inference
    print("Building module-to-task mapping...")
    module_to_tasks = build_module_to_tasks_map(canonical)
    print(f"  Found {len(module_to_tasks)} task modules")
    
    parallel, workers = get_parallel_settings()
    if parallel and workers > 1 and canonical:
        with ThreadPoolExecutor(max_workers=workers) as executor:
            enriched = list(executor.map(lambda it: enrich_item(it, module_to_tasks, dep_graph), canonical))
    else:
        enriched = [enrich_item(item, module_to_tasks, dep_graph) for item in canonical]
    
    # Write enriched output
    ENRICHED_JSON.parent.mkdir(parents=True, exist_ok=True)
    with open(ENRICHED_JSON, "w", encoding="utf-8") as f:
        json.dump(enriched, f, indent=4, ensure_ascii=False)
    
    # Count suggested dependencies
    total_suggested = sum(len(item.get("suggestedDependencies", [])) for item in enriched)
    print(f"✓ Wrote {len(enriched)} enriched items to {ENRICHED_JSON}")
    print(f"  Added {total_suggested} suggested dependencies from Agda dependency graph")

if __name__ == "__main__":
    enrich_canonical()
