#!/usr/bin/env python3
"""Tests for GP roadmap Agda renderer."""

from scripts.shared.gp_roadmap_render import render_roadmap_step


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
