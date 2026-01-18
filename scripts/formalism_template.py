#!/usr/bin/env python3
"""Generate Agda template stubs from the formalism adapter."""

from __future__ import annotations

from pathlib import Path
import sys

ROOT = Path(__file__).resolve().parent.parent
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

from scripts.shared.formalism_adapter import load_formalism_adapter


def render_template(adapter_path: Path) -> str:
    adapter = load_formalism_adapter(adapter_path)
    lines = [
        "{-# OPTIONS --without-K #-}",
        "",
        "-- | Auto-generated construction stubs (formalism adapter).",
        "module Plan.CIM.FormalismTemplates where",
        "",
        "open import Metamodel as M",
        "",
        f"-- Formalism: {adapter.formalism_id} (v{adapter.version})",
        "",
    ]

    for item in adapter.constructions:
        name = item.construction_id.replace(" ", "")
        lines.append(f"-- {item.kind}: {item.construction_id}")
        lines.append(f"{name}Construction : M.Identifier")
        lines.append(f"{name}Construction = M.mkId \"{item.construction_id}\"")
        lines.append("")

    return "\n".join(lines).rstrip() + "\n"


def main() -> None:
    adapter_path = ROOT / "data" / "formalisms" / "formalism_adapter.json"
    output_path = ROOT / "build" / "formalisms" / "construction_templates.agda"

    if not adapter_path.exists():
        raise SystemExit(f"Missing formalism adapter: {adapter_path}")

    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(render_template(adapter_path), encoding="utf-8")
    print(f"✓ Wrote Agda templates to {output_path}")


if __name__ == "__main__":
    main()
