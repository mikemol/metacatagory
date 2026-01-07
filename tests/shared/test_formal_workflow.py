from pathlib import Path
import textwrap

import pytest

from scripts.shared.formal_workflow import FormalWorkflow, WorkflowPolicy
from scripts.shared.errors import ValidationError


def write_agda(tmp_path: Path, name: str, content: str) -> Path:
    p = tmp_path / f"{name}.agda"
    p.write_text(textwrap.dedent(content))
    return p


def test_analyze_workspace_and_no_cycles(tmp_path: Path):
    # Create simple acyclic modules
    write_agda(tmp_path, "A", """
    module A where
    open import B
    """)
    write_agda(tmp_path, "B", """
    module B where
    """)

    fw = FormalWorkflow(src_dir=tmp_path)
    stats = fw.analyze_workspace()
    assert stats["total_modules"] == 2

    # No cycles expected, strict policy should pass
    res = fw.verify_no_cycles()
    assert res.is_valid()


def test_cycle_detection_strict_raises(tmp_path: Path):
    # Create cycle A -> B -> A
    write_agda(tmp_path, "A", """
    module A where
    open import B
    """)
    write_agda(tmp_path, "B", """
    module B where
    open import A
    """)

    fw = FormalWorkflow(src_dir=tmp_path, policy=WorkflowPolicy(strict=True))
    fw.analyze_workspace()
    with pytest.raises(ValidationError):
        fw.verify_no_cycles()


def test_generate_verified_report(tmp_path: Path):
    write_agda(tmp_path, "A", """
    module A where
    """)
    fw = FormalWorkflow(src_dir=tmp_path)
    fw.analyze_workspace()
    out = tmp_path / "formal_report"
    report = fw.generate_verified_report(output_path=out)
    # JSON file emitted
    assert out.with_suffix('.json').exists()
    # Basic structure
    assert "workspace" in report and "coverage" in report and "provenance" in report


def test_roundtrip_serialization(tmp_path: Path):
    write_agda(tmp_path, "A", """
    module A where
    """)
    fw = FormalWorkflow(src_dir=tmp_path)
    fw.analyze_workspace()
    data = fw.to_dict()
    fw2 = FormalWorkflow.from_dict(data)
    assert Path(fw2.src_dir) == tmp_path
