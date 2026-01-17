#!/usr/bin/env python3
"""Tests for GP roadmap Agda renderer."""

from scripts.shared.gp_roadmap_render import (
    build_implication,
    build_implication_from_concepts,
    build_step_summary,
    render_roadmap_step,
)


def test_render_roadmap_step_basic():
    rendered = render_roadmap_step(
        gp_id="GP01",
        title="Title",
        step="Step",
        implication="Implication",
        target_module="src/agda/Plan/CIM/Utility.agda",
    )
    assert "roadmapGp01" in rendered
    assert 'provenance   = "GP01: Title"' in rendered
    assert 'step         = "Step"' in rendered


def test_build_implication_helpers():
    metadata = {"insight": "I", "gap": "G", "fix": "F"}
    implication = build_implication(metadata)
    assert "Insight:" in implication and "Gap:" in implication and "Fix:" in implication

    summary = build_step_summary({"summary": "Do thing"})
    assert summary == "Do thing"

    clause = build_implication_from_concepts(["Alpha", "Beta"])
    assert clause == "Concepts: Alpha, Beta"
