"""
SPPF-Composable Onboarding Header

Roadmap: src/agda/Plan/CIM/Utility.agda
Architecture: ARCHITECTURE.md
Onboarding: COPILOT_SYNERGY.md

Constructive Proof Semantics:
- This script participates in the composable SPPF model, mirroring Agda record patterns for protocol,
    witness, and universal property semantics.
- All logic should be traceable to roadmap nodes and architectural principles.
- For onboarding, review the architecture and roadmap, and recursively revisit related nodes for
    context and composability.
"""
import json
from typing import Any

# Load badge script output (e.g., deferred-files.json)
badge_data: dict[str, dict[str, Any]]
with open(".github/badges/deferred-files.json") as f:
    badge_data = json.load(f)

# Load Agda registry export
agda_registry: list[dict[str, Any]]
with open("agda-technicaldebt.json") as f:
    agda_registry = json.load(f)

# Extract all annotated IDs from Agda registry
agda_ids: set[Any] = set(item["id"] for item in agda_registry)

# Find all postulates/TODOs/FIXMEs in badge data
unannotated: list[dict[str, Any]] = []
for filename, data in badge_data.items():
    for key in ["postulates", "todo", "fixme"]:
        count: int = data.get(key, 0)
        if count > 0:
            # For each, check if annotated in Agda registry
            # (You may want to extract actual identifiers from source for finer granularity)
            if filename not in agda_ids:
                unannotated.append({"file": filename, "type": key, "count": count})

# Report unannotated debt
print("Unannotated technical debt items:")
for item in unannotated:
    print(f"{item['file']}: {item['type']} ({item['count']})")
