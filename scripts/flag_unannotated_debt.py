# flag_unannotated_debt.py
"""
Script to flag unannotated technical debt in Agda registries.
"""
import sys
import re

def flag_unannotated(filename):
    with open(filename) as f:
        lines = f.readlines()
    for i, line in enumerate(lines):
        if 'TODO' in line or 'FIXME' in line:
            print(f"Line {i+1}: {line.strip()}")

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: flag_unannotated_debt.py <agda_file>")
        sys.exit(1)
    flag_unannotated(sys.argv[1])
