import json

# Load badge script output (e.g., deferred-files.json)
with open(".github/badges/deferred-files.json") as f:
    badge_data = json.load(f)

# Load Agda registry export
with open("agda-technicaldebt.json") as f:
    agda_registry = json.load(f)

# Extract all annotated IDs from Agda registry
agda_ids = set(item["id"] for item in agda_registry)

# Find all postulates/TODOs/FIXMEs in badge data
unannotated = []
for filename, data in badge_data.items():
    for key in ["postulates", "todo", "fixme"]:
        count = data.get(key, 0)
        if count > 0:
            # For each, check if annotated in Agda registry
            # (You may want to extract actual identifiers from source for finer granularity)
            if filename not in agda_ids:
                unannotated.append({"file": filename, "type": key, "count": count})

# Report unannotated debt
print("Unannotated technical debt items:")
for item in unannotated:
    print(f"{item['file']}: {item['type']} ({item['count']})")
