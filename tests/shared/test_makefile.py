#!/usr/bin/env python3

from scripts.shared.makefile import parse_make_database, parse_phony_targets, reachable_nodes


def test_parse_make_database_and_phony():
    text = """
.PHONY: foo bar

foo: dep1 dep2
bar: dep2 \\
 dep3
dep2:
"""
    graph = parse_make_database(text)
    assert graph["foo"] == {"dep1", "dep2"}
    assert graph["bar"] == {"dep2", "dep3"}
    assert parse_phony_targets(graph) == {"foo", "bar"}


def test_reachable_nodes():
    graph = {
        "a": {"b"},
        "b": {"c"},
        "c": set(),
    }
    assert reachable_nodes(["a"], graph) == {"a", "b", "c"}
