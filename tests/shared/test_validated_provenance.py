import json
import pytest

from scripts.shared.validated_provenance import ValidatedProvenance, ValidationPolicy
from scripts.shared.provenance import ProvenanceTracker
from scripts.shared.errors import ValidationError


def test_add_valid_record_creates_trail():
    vp = ValidatedProvenance(system_id="sys")
    rec = {
        "source_type": "script",
        "source_id": "builder",
        "metadata": {"description": "compiled"},
    }
    vp.add_validated_record("artifact-1", rec)

    trail = vp.tracker.get_trail("artifact-1")
    assert trail is not None
    assert len(trail.records) == 1
    assert trail.records[0].source_id == "builder"


def test_invalid_record_raises_validation_error():
    vp = ValidatedProvenance(system_id="sys")
    bad = {"source_type": "script"}  # missing source_id
    with pytest.raises(ValidationError) as err:
        vp.add_validated_record("a1", bad)
    e = err.value
    assert isinstance(e, ValidationError)
    assert e.recoverable is True
    assert e.context.get("field") == "record"


def test_lineage_missing_related_detected():
    vp = ValidatedProvenance(system_id="sys")
    # Create B and link A -> B without creating A trail
    vp.tracker.create_trail("B")
    vp.tracker.link_artifacts("A", "B")

    res = vp.validate_lineage_consistency("B")
    assert not res.is_valid()
    assert res.to_dict()["error_count"] >= 1


def test_generate_validated_report_strict_raises_on_invalid_lineage():
    vp = ValidatedProvenance(system_id="sys")
    vp.tracker.create_trail("B")
    vp.tracker.link_artifacts("A", "B")
    with pytest.raises(ValidationError):
        vp.generate_validated_report()


def test_generate_validated_report_writes_files(tmp_path):
    # Non-strict lineage policy to allow writing
    policy = ValidationPolicy(strict=False)
    vp = ValidatedProvenance(system_id="sys", policy=policy)
    vp.add_validated_record("X", {"source_type": "script", "source_id": "seed"})
    out = tmp_path / "prov_validated"
    summary = vp.generate_validated_report(output_path=out)
    # JSON and MD should be written
    json_path = out.with_suffix(".json")
    md_path = out.with_suffix(".md")
    assert json_path.exists()
    assert md_path.exists()
    # summary structure
    assert "report" in summary and "validation" in summary


def test_roundtrip_to_from_dict():
    vp = ValidatedProvenance(system_id="sys")
    vp.add_validated_record("A", {"source_type": "script", "source_id": "seed"})
    vp.tracker.link_artifacts("A", "B")
    vp.tracker.create_trail("B")

    data = vp.to_dict()
    vp2 = ValidatedProvenance.from_dict(data)
    # Compare core aspects
    assert vp2.tracker.system_id == "sys"
    assert set(vp2.tracker.trails.keys()) == set(vp.tracker.trails.keys())
    assert len(vp2.tracker.get_trail("A").records) == 1
