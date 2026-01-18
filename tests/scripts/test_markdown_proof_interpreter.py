#!/usr/bin/env python3
"""Tests for markdown_proof_interpreter.py."""

import json

import scripts.markdown_proof_interpreter as mod


def test_load_ast_and_proof(tmp_path):
    ast_data = {"blocks": [], "meta": {}}
    proof_data = [{"rule": "normalize_list", "target": None}]

    ast_path = tmp_path / "ast.json"
    proof_path = tmp_path / "proof.json"

    ast_path.write_text(json.dumps(ast_data))
    proof_path.write_text(json.dumps(proof_data))

    assert mod.load_ast(str(ast_path)) == ast_data
    assert mod.load_proof(str(proof_path)) == proof_data


def test_apply_proof_dispatches(monkeypatch):
    ast = {"blocks": []}
    proof = [
        {"rule": "normalize_list", "target": "list", "params": {"style": "bullet"}},
        {"rule": "unknown_rule", "target": "noop"},
    ]

    def fake_normalize_list(current, target, params):
        assert current == ast
        assert target == "list"
        assert params == {"style": "bullet"}
        return {"blocks": ["normalized"]}

    monkeypatch.setattr(mod, "normalize_list", fake_normalize_list)

    result = mod.apply_proof(ast, proof)
    assert result == {"blocks": ["normalized"]}
