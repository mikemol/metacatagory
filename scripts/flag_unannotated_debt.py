"""
Flag unannotated technical debt by comparing the badge scan (per-file counts)
with the Agda debt registry (if present). Run after `make badges`.
"""

from pathlib import Path
from typing import Any, Dict, List

from scripts.shared.io import load_json

DEFERRED_FILES = Path(".github/badges/deferred-files.json")
AGDA_REGISTRY = Path("agda-technicaldebt.json")

def main() -> None:
    if not DEFERRED_FILES.exists():
        print("deferred-files.json not found; run `make badges` first.")
        return

    badge_data: Dict[str, Dict[str, Any]] = load_json(
        DEFERRED_FILES,
        required=True,
        error_msg=f"Missing deferred files report: {DEFERRED_FILES}",
    )

    agda_ids: set[Any] = set()
    if AGDA_REGISTRY.exists():
        agda_registry: List[Dict[str, Any]] = load_json(
            AGDA_REGISTRY,
            required=True,
            error_msg=f"Missing Agda debt registry: {AGDA_REGISTRY}",
        )
        agda_ids = set(item.get("id") for item in agda_registry)

    unannotated: List[Dict[str, Any]] = []
    for filename, data in badge_data.items():
        for key in ["postulates", "todo", "fixme"]:
            count: int = data.get(key, 0)
            if count > 0 and filename not in agda_ids:
                unannotated.append({"file": filename, "type": key, "count": count})

    if not unannotated:
        print("All scanned debt is annotated in the registry (or no debt found).")
    else:
        print("Unannotated technical debt items:")
        for item in unannotated:
            print(f"{item['file']}: {item['type']} ({item['count']})")

if __name__ == "__main__":
    main()
