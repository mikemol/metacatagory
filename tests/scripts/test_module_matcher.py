import textwrap
from pathlib import Path

from scripts import shared_data
from scripts.module_matcher import ModuleMatcher


def test_extract_keywords_uses_declarations_and_comments(tmp_path: Path) -> None:
    agda_file = tmp_path / "Sample.agda"
    agda_file.write_text(
        textwrap.dedent(
            """\
            module Sample where

            -- This comment mentions CategoryTheory and coherence
            record MyRecord : Set where
              field
                value : Set
            """
        ),
        encoding="utf-8",
    )

    matcher = ModuleMatcher(workspace_root=str(tmp_path))
    content = agda_file.read_text(encoding="utf-8")
    keywords = matcher._extract_keywords(content, agda_file)

    assert "myrecord" in keywords
    assert "comment" in keywords


def test_match_roadmap_to_modules_uses_validated_loader(
    tmp_path: Path, monkeypatch
) -> None:
    matcher = ModuleMatcher(workspace_root=str(tmp_path))
    called = {}

    def fake_loader(repo_root: Path, filter_legacy: bool = True):
        called["ok"] = True
        return [
            {
                "id": "GP1",
                "title": "Test",
                "category": "meta",
                "tags": [],
                "files": [],
            }
        ]

    monkeypatch.setattr(shared_data, "load_planning_index_validated", fake_loader)

    mappings = matcher.match_roadmap_to_modules(metadata_file="build/ingested_metadata.json")

    assert called.get("ok") is True
    assert "GP1" in mappings
