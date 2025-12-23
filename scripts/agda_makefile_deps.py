#!/usr/bin/env python3
import os
import re
from pathlib import Path

AGDA_SRC = Path("src/agda")
MODULE_RE = re.compile(r"^(?:open\s+)?import\s+([A-Za-z0-9_.]+)")


# Map Agda module names to file paths
def module_to_path(module):
    return AGDA_SRC / Path(module.replace(".", "/")).with_suffix(".agda")


def find_agda_files():
    return list(AGDA_SRC.rglob("*.agda"))


def parse_imports(agda_file):
    imports = set()
    with open(agda_file, "r") as f:
        for line in f:
            m = MODULE_RE.match(line.strip())
            if m:
                imports.add(m.group(1))
    return imports


def makefile_rule(agda_file, imports):
    agdai = str(agda_file.with_suffix(".agdai"))
    deps = [str(agda_file)]
    for mod in imports:
        dep_path = module_to_path(mod)
        if dep_path.exists():
            deps.append(str(dep_path))
    deps_str = " ".join(deps)
    rule = f"{agdai}: {deps_str}\n\tagda -i src/agda {agda_file}\n"
    return rule


def main():
    for agda_file in find_agda_files():
        imports = parse_imports(agda_file)
        rule = makefile_rule(agda_file, imports)
        print(rule)


if __name__ == "__main__":
    main()
