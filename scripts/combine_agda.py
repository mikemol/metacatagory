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
import os

output_file = "combined.agda.txt"
root_dir = "src/agda"

with open(output_file, "w", encoding="utf-8") as out:
    for dirpath, _, filenames in os.walk(root_dir):
        for fname in filenames:
            if fname.endswith(".agda"):
                fpath = os.path.join(dirpath, fname)
                relpath = os.path.relpath(fpath, root_dir)
                with open(fpath, "r", encoding="utf-8", errors="ignore") as f:
                    lines = f.readlines()
                # Extract module name (first non-comment line starting with 'module')
                modname = None
                for line in lines:
                    if line.strip().startswith("module "):
                        modname = line.strip().split()[1]
                        break
                out.write(f"=== {modname or 'UNKNOWN'} | {relpath} ===\n")
                out.writelines(lines)
                out.write("\n\n")
print(f"Unified Agda code written to {output_file}")
