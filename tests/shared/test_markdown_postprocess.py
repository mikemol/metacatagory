#!/usr/bin/env python3

from scripts.shared.markdown import (
    is_json_input,
    fix_list_markers,
    remove_multiple_blank_lines,
    fix_horizontal_rules,
    fix_headings,
    postprocess_markdown,
)


def test_is_json_input() -> None:
    assert is_json_input('{"a": 1}') is True
    assert is_json_input("[1,2,3]") is True
    assert is_json_input("not json") is False


def test_fix_list_markers() -> None:
    md = "- a\n  - b\n"
    fixed = fix_list_markers(md)
    assert "* a" in fixed
    assert "  * b" in fixed


def test_remove_multiple_blank_lines() -> None:
    md = "\n\nline\n\n\n\nline2\n"
    cleaned = remove_multiple_blank_lines(md)
    assert cleaned.startswith("line")
    assert "\n\n\n" not in cleaned


def test_fix_horizontal_rules() -> None:
    md = "-----\n"
    fixed = fix_horizontal_rules(md)
    assert fixed.strip() == "-" * 79


def test_fix_headings() -> None:
    md = "Title\n=====\n"
    fixed = fix_headings(md)
    assert fixed.strip().startswith("# Title")


def test_postprocess_markdown() -> None:
    md = "\n\n- a\n\n\n-----\n"
    cleaned = postprocess_markdown(md)
    assert "##" in cleaned
