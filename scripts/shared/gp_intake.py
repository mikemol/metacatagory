#!/usr/bin/env python3
"""Shared intake/GP parsing utilities."""

from __future__ import annotations

import json
import re
from pathlib import Path
from typing import Dict, List, Optional


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
        lines = [l.strip() for l in clean_content.split('\\n') if l.strip()]
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


def extract_metadata_from_md(filepath: Path | str) -> Dict:
    """Extract metadata from a markdown file."""
    path = Path(filepath)
    content = path.read_text(encoding="utf-8", errors="ignore")
    return extract_metadata_from_text(content, path.stem)


def load_concept_config(config_path: Path) -> Dict:
    """Load concept patterns and settings from config file."""
    if config_path.exists():
        with open(config_path) as f:
            return json.load(f)
    return {
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
