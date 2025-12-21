
#!/bin/bash
# Export CNF grammar from Agda module to JSON file using IO main
AGDA_MODULE="src/agda/Plan/CIM/GrammarBridge.agda"
OUTPUT_JSON="sample_cnf_grammar.json"

# Compile the Agda module (main will write the JSON file)
agda --compile --include-path=src/agda "$AGDA_MODULE"

# Run the compiled executable to produce the JSON file
./src/agda/Plan/CIM/GrammarBridge

echo "CNF grammar exported to $OUTPUT_JSON via Agda IO main."
