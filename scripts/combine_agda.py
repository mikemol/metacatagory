# combine_agda.py
"""
Script to combine Agda files for registry automation.
"""
import sys
import os

def combine_agda_files(input_files, output_file):
    with open(output_file, 'w') as out:
        for fname in input_files:
            with open(fname) as f:
                out.write(f.read())
                out.write('\n')

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: combine_agda.py <output_file> <input_file1> [<input_file2> ...]")
        sys.exit(1)
    output_file = sys.argv[1]
    input_files = sys.argv[2:]
    combine_agda_files(input_files, output_file)
