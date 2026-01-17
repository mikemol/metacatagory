#!/usr/bin/env python3

from scripts.shared.agda_adapters import parse_adapter_record, count_adapter_records


def test_parse_adapter_record_basic() -> None:
    lines = [
        "record FooAdapter : Set where\n",
        "  field\n",
        "    decl : FooDecl\n",
        "    status : Bool\n",
        "mkFooAdapter : FooDecl -> FooAdapter\n",
    ]
    record = parse_adapter_record(lines, 0)
    assert record is not None
    assert record.name == "FooAdapter"
    assert record.decl_type == "FooDecl"
    assert record.has_status is True
    assert record.constructor_name == "mkFooAdapter"


def test_count_adapter_records() -> None:
    content = "\n".join(
        [
            "record FooAdapter : Set where",
            "record BarAdapter : Set where",
            "record Baz : Set where",
        ]
    )
    assert count_adapter_records(content) == 2
