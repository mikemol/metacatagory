import textwrap
from pathlib import Path

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
