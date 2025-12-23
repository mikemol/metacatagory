#!/usr/bin/env python3
"""
cnf_grammar_interpreter.py

Interpreter for CNF grammar and SPPF exported from Agda.
Loads a CNF grammar (as JSON) and applies it to a Pandoc AST or proof trace.
"""

import sys
import json
from typing import Any


# --- Load CNF grammar ---
def load_cnf_grammar(path: str) -> dict[str, Any]:
    with open(path) as f:
        return json.load(f)


# --- Example: Apply CNF grammar to a proof trace ---
def apply_cnf_grammar(
    grammar: dict[str, Any], proof_trace: dict[str, Any]
) -> dict[str, Any]:
    # For demo: just return the rules and steps
    return {
        "rules": grammar.get("rules", []),
        "steps": proof_trace.get("steps", []),
        "summary": proof_trace.get("summary", ""),
    }


# --- Main entry point ---
def main():
    if len(sys.argv) != 3:
        print("Usage: cnf_grammar_interpreter.py <cnf_grammar.json> <proof_trace.json>")
        sys.exit(1)
    grammar = load_cnf_grammar(sys.argv[1])
    proof_trace = load_cnf_grammar(sys.argv[2])
    result = apply_cnf_grammar(grammar, proof_trace)
    print(json.dumps(result, indent=2))


if __name__ == "__main__":
    main()
