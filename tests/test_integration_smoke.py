import importlib.util
import json
from pathlib import Path

from scripts.shared_yaml import dump_yaml


def load_module(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    assert spec and spec.loader
    spec.loader.exec_module(module)
    return module


def test_merge_roadmaps_minimal(tmp_path):
    repo_root = tmp_path
    tasks_dir = repo_root / ".github" / "roadmap"
    tasks_dir.mkdir(parents=True, exist_ok=True)
    (tasks_dir / "tasks.json").write_text(
        json.dumps(
            [
                {
                    "id": "T-1",
                    "title": "Task Alpha",
                    "description": "Alpha description",
                    "status": "planned",
                    "category": "demo",
                    "source": "tasks.json",
                    "files": ["src/alpha.agda"],
                    "tags": ["demo"],
                    "dependsOn": [],
                    "related": [],
                    "provenance": [],
                }
            ]
        )
    )

    roadmap_md = repo_root / "ROADMAP.md"
    fm = {
        "id": "RM-1",
        "title": "Task Beta",
        "description": "Beta description",
        "status": "planned",
        "category": "demo",
        "files": ["src/beta.agda"],
    }
    roadmap_md.write_text(f"```yaml\n{dump_yaml(fm)}\n```\n")

    ingested_dir = repo_root / "src" / "agda" / "Plan" / "CIM" / "IngestedRoadmaps"
    ingested_dir.mkdir(parents=True, exist_ok=True)
    ingested_dir.joinpath("RoadmapGp01.agda").write_text(
        """
open import Agda.Builtin.String

roadmapGp01 : RoadmapStep
roadmapGp01 = record
  { provenance = \"GP provenance\"
  ; step = \"Task Gamma\"
  ; status = \"completed\"
  ; targetModule = \"src/gamma.agda\"
  }
"""
    )

    merge_mod = load_module("merge_roadmaps_module", Path(__file__).parents[1] / "scripts" / "merge_roadmaps.py")
    items = merge_mod.merge_all_sources(repo_root)
    assert len(items) == 3

    out_json = repo_root / "build" / "canonical_roadmap.json"
    merge_mod.export_to_json(items, out_json)
    assert out_json.exists()
    loaded = json.loads(out_json.read_text())
    assert len(loaded) == len(items)

    out_agda = repo_root / "build" / "canonical_roadmap.agda"
    merge_mod.export_to_agda(items, out_agda)
    assert out_agda.exists()
    assert "canonicalItems" in out_agda.read_text()


def test_export_roadmap_sppf_minimal(tmp_path, monkeypatch):
    repo_root = tmp_path
    planning = repo_root / "data" / "planning_index.json"
    planning.parent.mkdir(parents=True, exist_ok=True)
    planning.write_text(
        json.dumps(
            [
                {
                    "id": "N1",
                    "title": "Node One",
                    "status": "not-started",
                    "category": "demo",
                    "source": "fixture",
                    "files": ["src/one.agda"],
                    "tags": ["demo"],
                }
            ]
        )
    )

    sppf_mod = load_module("export_roadmap_sppf_module", Path(__file__).parents[1] / "scripts" / "export_roadmap_sppf.py")
    monkeypatch.chdir(repo_root)
    sppf_mod.main()

    out_path = repo_root / "build" / "gp_roadmap_sppf.json"
    assert out_path.exists()
    data = json.loads(out_path.read_text())
    assert data["nodes"][0]["id"] == "N1"


def test_export_dependency_graph_minimal(tmp_path):
    repo_root = tmp_path
    enriched = repo_root / "build" / "canonical_enriched.json"
    enriched.parent.mkdir(parents=True, exist_ok=True)
    enriched.write_text(
        json.dumps(
            [
                {
                    "id": "A",
                    "title": "Task A",
                    "status": "planned",
                    "category": "demo",
                    "dependsOn": [],
                    "suggestedDependencies": [],
                },
                {
                    "id": "B",
                    "title": "Task B",
                    "status": "planned",
                    "category": "demo",
                    "dependsOn": ["A"],
                    "suggestedDependencies": ["A"],
                },
            ]
        )
    )

    dep_mod = load_module("export_dependency_graph_module", Path(__file__).parents[1] / "scripts" / "export_dependency_graph.py")
    dep_mod.ENRICHED_JSON = enriched
    dep_mod.MERMAID_OUTPUT = repo_root / "build" / "reports" / "dependency_graph.mmd"
    dep_mod.DOT_OUTPUT = repo_root / "build" / "reports" / "dependency_graph.dot"

    dep_mod.export_dependency_graphs()

    assert dep_mod.MERMAID_OUTPUT.exists()
    assert dep_mod.DOT_OUTPUT.exists()
    assert "TaskDependencies" in dep_mod.DOT_OUTPUT.read_text()
