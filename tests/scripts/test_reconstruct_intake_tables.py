#!/usr/bin/env python3

from pathlib import Path

from scripts.reconstruct_intake_tables import list_intake_files, process_file


def test_list_intake_files_filters_numeric(tmp_path: Path) -> None:
    intake_dir = tmp_path / "intake"
    intake_dir.mkdir()
    (intake_dir / "__01.md").write_text("ok", encoding="utf-8")
    (intake_dir / "__99.md").write_text("ok", encoding="utf-8")
    (intake_dir / "__abc.md").write_text("skip", encoding="utf-8")

    files = list_intake_files(intake_dir)
    names = {f.name for f in files}

    assert "__01.md" in names
    assert "__99.md" in names
    assert "__abc.md" not in names


def test_process_file_no_table_changes(tmp_path: Path) -> None:
    file_path = tmp_path / "__01.md"
    original = "Just text\nNo tables here\n"
    file_path.write_text(original, encoding="utf-8")

    process_file(file_path)

    assert file_path.read_text(encoding="utf-8") == original
