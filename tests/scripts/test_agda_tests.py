#!/usr/bin/env python3

from scripts.shared.agda_tests import (
    parse_total_assertions,
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


def test_infer_section_from_preceding() -> None:
    content = "Level2sub5 blah blah chk2s7"
    assert infer_section_from_preceding(content) == "5"
    assert infer_section_from_preceding("chk9s3") == "3"
    assert infer_section_from_preceding("no markers") == "0"
