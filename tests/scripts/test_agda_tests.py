#!/usr/bin/env python3

from scripts.shared.agda_tests import (
    extract_chapter_from_filename,
    extract_sections_from_content,
    parse_total_assertions,
    iter_checklist_links,
    iter_checklist_adapters,
    infer_section_from_preceding,
)


def test_parse_total_assertions() -> None:
    content = "totalAssertions ≡ 42"
    assert parse_total_assertions(content) == 42
    assert parse_total_assertions("no match") is None


def test_iter_checklist_adapters() -> None:
    content = "\n".join(
        [
            "foo-adapter : A.Bar",
            "baz-adapter : A.Bip",
        ]
    )
    adapters = iter_checklist_adapters(content)
    assert ("foo", "Bar", 0) in adapters
    assert any(name == "baz" and typ == "Bip" for name, typ, _ in adapters)


def test_extract_chapter_from_filename() -> None:
    assert extract_chapter_from_filename("Chapter2Checklist") == "Chapter2"
    assert extract_chapter_from_filename("NotAChapter") is None


def test_extract_sections_from_content() -> None:
    content = "\n".join(
        [
            "----",
            "-- Level1sub3",
            "text",
            "----",
            "-- Level1sub10",
        ]
    )
    assert extract_sections_from_content(content) == ["3", "10"]


def test_iter_checklist_links() -> None:
    content = "\n".join(
        [
            "foo-bar-link : X ≡ Target.One",
            "baz-qux-link : Y ≡ Target.Two",
        ]
    )
    links = iter_checklist_links(content)
    assert ("foo", "Target.One") in links
    assert ("baz", "Target.Two") in links


def test_infer_section_from_preceding() -> None:
    content = "Level2sub5 blah blah chk2s7"
    assert infer_section_from_preceding(content) == "5"
    assert infer_section_from_preceding("chk9s3") == "3"
    assert infer_section_from_preceding("no markers") == "0"
