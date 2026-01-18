#!/usr/bin/env python3
"""Shared intake/GP parsing utilities."""

from __future__ import annotations

import json
import re
from pathlib import Path
from typing import Dict, List, Optional

from scripts.shared.io import load_json


def load_gp_text(path: Path) -> str:
    """Load GP markdown content with consistent encoding handling."""
    return path.read_text(encoding="utf-8", errors="ignore")

def strip_base64_images(text: str) -> str:
    """Remove Base64-encoded images from text to prevent pollution."""
    text = re.sub(r'data:image/[^;]+;base64,[A-Za-z0-9+/=]+', '[image removed]', text)
    text = re.sub(r'!\[.*?\]\(data:image[^)]+\)', '[image removed]', text)
    return text


def extract_structured_sections(content: str) -> Dict[str, str]:
    """Extract Insight/Gap/Fix sections from a GP Markdown snippet."""
    section_re = re.compile(r'^\s*\*\s+\*\*The (Insight|Gap|Fix):\*\s*(.*)', re.IGNORECASE)
    sections: Dict[str, List[str]] = {}
    current: str | None = None

    for line in content.splitlines():
        heading_match = section_re.match(line)
        if heading_match:
            label = heading_match.group(1).lower()
            sections[label] = [heading_match.group(2).strip()]
            current = label
            continue

        if current:
            if re.match(r'^\s*#{1,6}', line):
                current = None
                continue
            stripped = line.strip()
            if stripped.startswith('* *The '):
                current = None
                continue
            if stripped:
                cleaned = stripped.lstrip('> ').strip()
                sections.setdefault(current, []).append(cleaned)

    return {
        label: ' '.join(lines).replace('\n', ' ').strip()
        for label, lines in sections.items()
    }


def extract_metadata_from_text(content: str, fallback_title: str) -> Dict:
    """Extract title, summary, and key sections from GP markdown content."""
    content = strip_base64_images(content)

    title_match = re.search(r'^#{1,3}\s+(.+?)$', content, re.MULTILINE)
    title = title_match.group(1).strip() if title_match else fallback_title

    sections = extract_structured_sections(content)

    if sections.get('insight') or sections.get('gap') or sections.get('fix'):
        summary_parts = []
        if sections.get('insight'):
            summary_parts.append(f"Insight: {sections['insight'][:80]}")
        if sections.get('gap'):
            summary_parts.append(f"Gap: {sections['gap'][:80]}")
        if sections.get('fix'):
            summary_parts.append(f"Fix: {sections['fix'][:80]}")
        summary = ' | '.join(summary_parts)[:200]
    else:
        clean_content = re.sub(r'[#\\*_`\\[\\]]', '', content)
        lines = [l.strip() for l in clean_content.splitlines() if l.strip()]
        declarative_lines = [l for l in lines if not l.endswith('?')]
        filtered_lines = [
            l for l in declarative_lines
            if not re.search(r'would you like|shall I|do you want', l, re.IGNORECASE)
        ]
        summary = ' '.join(filtered_lines[:3] if filtered_lines else lines[:3])[:150]

    keywords = set()
    patterns = [
        r'(?:implementation|implement).*?(?:of|for)\\s+([^.]+)',
        r'(?:design|define)\\s+([^.]+)',
        r'(?:extends?|builds?|creates?)\\s+([^.]+)',
    ]
    for pattern in patterns:
        matches = re.findall(pattern, content, re.IGNORECASE)
        keywords.update(m.strip()[:50] for m in matches if m.strip())

    return {
        'title': title,
        'summary': summary,
        'keywords': list(keywords)[:3],
        'insight': sections.get('insight', ''),
        'gap': sections.get('gap', ''),
        'fix': sections.get('fix', ''),
    }


def extract_question(content: str) -> str:
    """Extract the actionable 'Would you like...' question."""
    lines = content.splitlines()
    for line in lines[:5]:
        if line.strip().startswith('Would you like'):
            question = line.strip()
            question = re.sub(r'\*\*(.+?)\*\*', r'\1', question)
            return question[:500]
    return "See full GP file for details"


def extract_formal_section(content: str) -> str:
    """Extract the formal correction/analysis section."""
    pattern = r'### \*?\*?I\. Formal (?:Correction|Analysis).*?\n\n(.+?)(?=\n###|\Z)'
    match = re.search(pattern, content, re.MULTILINE | re.DOTALL)
    if match:
        section = match.group(1).strip()
        if len(section) > 1000:
            section = section[:1000] + "..."
        return section
    return "See full GP file"


def extract_related_gps(content: str) -> List[str]:
    """Find references to other GP files."""
    related = set()
    gp_refs = re.findall(r'GP(\d+)', content)
    related.update(f"GP{num}" for num in gp_refs)
    return sorted(list(related))


def extract_manifest_version(content: str) -> Optional[str]:
    """Check if this GP introduces a new manifest version."""
    match = re.search(r'Manifest \(v([\d.]+)\)', content)
    return f"v{match.group(1)}" if match else None


def extract_target_modules(content: str) -> List[str]:
    """Find mentions of target implementation modules."""
    modules = set()
    agda_refs = re.findall(r'(\w+\.agda)', content)
    py_refs = re.findall(r'(\w+\.py)', content)
    modules.update(agda_refs)
    modules.update(py_refs)
    return sorted(list(modules))


def categorize_gp(gp_num: int) -> str:
    """Determine category based on GP number."""
    if gp_num <= 111:
        return "Foundation"
    if 200 <= gp_num < 400:
        return "Geometry"
    if 400 <= gp_num < 600:
        return "Corrections"
    if 699 <= gp_num < 800:
        return "Polytopes"
    return "Analysis"


def extract_gp_number(gp_id: str) -> int:
    """Extract the numeric portion of a GP identifier."""
    match = re.search(r"\d+", gp_id)
    return int(match.group()) if match else 0


def categorize_gp_phase(gp_num: int) -> str:
    """Categorize GP number into phase buckets for routing."""
    if gp_num < 100:
        return "foundational"
    if gp_num < 200:
        return "structural"
    if gp_num < 300:
        return "geometric"
    if gp_num < 400:
        return "topological"
    if gp_num < 500:
        return "homological"
    if gp_num < 600:
        return "polytope"
    if gp_num < 700:
        return "coherence"
    if gp_num < 800:
        return "analysis"
    return "unified"


def extract_metadata_from_md(filepath: Path | str) -> Dict:
    """Extract metadata from a markdown file."""
    path = Path(filepath)
    content = load_gp_text(path)
    return extract_metadata_from_text(content, path.stem)


def build_gp_metadata(content: str, gp_id: str, config: Dict | None = None) -> Dict:
    """Build a canonical metadata map for a GP entry."""
    config = config or load_concept_config(
        Path(__file__).resolve().parent.parent / "extract-concepts-config.json"
    )
    metadata = extract_metadata_from_text(content, gp_id)
    gp_num = extract_gp_number(gp_id)
    key_concepts = extract_concepts(content, config)

    return {
        **metadata,
        "category": categorize_gp(gp_num),
        "question": extract_question(content),
        "formal_correction": extract_formal_section(content),
        "related_gps": extract_related_gps(content),
        "manifest_version": extract_manifest_version(content),
        "target_modules": extract_target_modules(content),
        "key_concepts": key_concepts,
        "target_module": infer_target_module(
            content=content,
            title=metadata.get("title", gp_id),
            keywords=metadata.get("keywords", []),
            config=config,
        ),
    }


def load_concept_config(config_path: Path) -> Dict:
    """Load concept patterns and settings from config file."""
    default_config = {
        "concept_patterns": [
            "RoPE|SPPF|Stasheff|Associahedron|Polytope|Loday|Yoneda|Sheaf|Cohomology",
            "Braid|Homotopy|Functor|Adjunction|Manifold|Topology|Geometry",
            "Lie Group|Group Action|Category Theory|HoTT|Abelian",
            "Quaternion|Octonion|Complex|Vector Space|Tensor",
        ],
        "default_target_module": "src/agda/Plan/CIM/Implementation.agda",
        "max_concepts": 15,
        "concept_title_case": True,
    }
    return load_json(config_path, default=default_config)


def infer_target_module(content: str, title: str, keywords: List[str], config: Dict) -> str:
    """Route GP content to an Agda module based on heuristics."""
    content_lower = content.lower()
    title_lower = title.lower()
    all_text = f"{content_lower} {title_lower} {' '.join(keywords).lower()}"

    if re.search(r'\b(category|functor|morphism|natural transformation)\b', all_text):
        return "src/agda/Core/CategoricalAdapter.agda"
    if re.search(r'\b(field|algebra|ring|group)\b', all_text):
        if re.search(r'\bf2\b|\bfinite field\b|\bgalois\b', all_text):
            return "src/agda/Algebra/Fields/F2.agda"
        return "src/agda/Algebra/Fields/GenericField.agda"
    if re.search(r'\bpolynomial', all_text):
        if re.search(r'\bf2\b', all_text):
            return "src/agda/Core/PolynomialsF2.agda"
        return "src/agda/Algebra/Polynomials.agda"
    if re.search(r'\b(matrix|linear|vector|rotation)\b', all_text):
        return "src/agda/Algebra/LinearAlgebra.agda"
    if re.search(r'\b(storage|persist|serialize|database)\b', all_text):
        return "src/agda/Infrastructure/Storage.agda"
    if re.search(r'\b(polytope|geometry|visual|diagram|render)\b', all_text):
        return "src/agda/Plan/CIM/Polytopes.agda"

    return config.get("default_target_module", "src/agda/Plan/CIM/Implementation.agda")


def extract_concepts(content: str, config: Dict) -> List[str]:
    """Extract key mathematical concepts from the content."""
    concepts = set()
    concept_patterns = [r'\\b(' + p + ')' for p in config['concept_patterns']]
    max_concepts = config.get('max_concepts', 15)
    use_title_case = config.get('concept_title_case', True)

    for pattern in concept_patterns:
        matches = re.findall(pattern, content, re.IGNORECASE)
        if use_title_case:
            concepts.update(m.title() for m in matches)
        else:
            concepts.update(matches)

    return sorted(list(concepts))[:max_concepts]
