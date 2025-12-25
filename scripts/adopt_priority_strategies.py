#!/usr/bin/env python3
"""
PRIO-ADOPT-1: Align Python badge weight profiles with Agda TechnicalDebt.Priorities

Separation of Concerns:
1. LOGIC LAYER: Pure mapping from Agda priorities to Python weight categories
2. FORMAT LAYER: JSON serialization and file I/O

This ensures the core mapping logic is independent of serialization details.
"""

import json
from pathlib import Path
from typing import Dict, Any

# ============================================================================
# LOGIC LAYER: Pure Priority Mapping
# ============================================================================

# ============================================================================
# LOGIC LAYER: Pure Priority Mapping
# ============================================================================

"""
Mapping Strategy (domain logic - independent of serialization):
- postulate ← proof        (Proof obligations, unimplemented algorithms)
- todo ← documentation    (Documentation TODOs, planned features)  
- fixme ← safety          (Safety issues, bugs, performance problems)
- deviation ← critical    (Critical deviations, specification mismatches)
"""

# Agda Priority values (in positive integers representing weight/urgency)
AGDA_STRATEGIES = {
    "default": {
        "minimal": 1,
        "low": 10,
        "medium": 50,
        "high": 100,
        "critical": 500,
        "testFixture": 1,
        "documentation": 25,
        "performance": 75,
        "safety": 200,
        "proof": 150,
    },
    "ffiSafety": {
        "minimal": 1,
        "low": 5,
        "medium": 25,
        "high": 50,
        "critical": 1000,
        "testFixture": 1,
        "documentation": 10,
        "performance": 100,
        "safety": 800,
        "proof": 30,
    },
    "proofCompleteness": {
        "minimal": 1,
        "low": 10,
        "medium": 50,
        "high": 200,
        "critical": 500,
        "testFixture": 1,
        "documentation": 20,
        "performance": 30,
        "safety": 150,
        "proof": 300,
    },
    "rapidDevelopment": {
        "minimal": 1,
        "low": 5,
        "medium": 20,
        "high": 50,
        "critical": 100,
        "testFixture": 1,
        "documentation": 5,
        "performance": 10,
        "safety": 75,
        "proof": 25,
    },
    "production": {
        "minimal": 10,
        "low": 50,
        "medium": 150,
        "high": 300,
        "critical": 1000,
        "testFixture": 5,
        "documentation": 100,
        "performance": 200,
        "safety": 500,
        "proof": 400,
    },
}

# ============================================================================
# Semantic Mapping: Agda Priority Fields → Python Badge Categories
# ============================================================================

def agda_to_python_weights(strategy_name: str) -> Dict[str, float]:
    """
    LOGIC LAYER: Pure mapping from Agda PriorityStrategy to Python weight categories.
    
    No side effects, no I/O, pure computation.
    
    Mapping:
    - postulate: from proof field (proof obligations as primary postulate category)
    - todo: from documentation field (doc TODOs as primary todo category)
    - fixme: from safety field (safety issues require fixes)
    - deviation: from critical field (critical deviations as specification mismatches)
    """
    if strategy_name not in AGDA_STRATEGIES:
        raise ValueError(f"Unknown strategy: {strategy_name}. Available: {list(AGDA_STRATEGIES.keys())}")
    
    agda_strat = AGDA_STRATEGIES[strategy_name]
    
    # Map Agda priorities to Python weight categories
    # Normalize to rough float scales (Python weights typically range 0.8 - 5.0)
    return {
        "postulate": agda_strat["proof"] / 100.0,        # 150 → 1.5, 300 → 3.0, etc
        "todo": agda_strat["documentation"] / 50.0,      # 25 → 0.5, 10 → 0.2, etc
        "fixme": agda_strat["safety"] / 100.0,           # 200 → 2.0, 800 → 8.0, etc
        "deviation": agda_strat["critical"] / 500.0,     # 500 → 1.0, 1000 → 2.0, etc
    }


def build_weight_profiles() -> Dict[str, Any]:
    """
    LOGIC LAYER: Compute all weight profiles from all strategies.
    
    Pure function: no I/O, no side effects.
    """
    profiles = {}
    for strategy_name in AGDA_STRATEGIES.keys():
        profiles[strategy_name] = agda_to_python_weights(strategy_name)
    
    return profiles


# ============================================================================
# FORMAT LAYER: JSON Serialization
# ============================================================================

def format_weights_json(profiles: Dict[str, Any]) -> Dict[str, Any]:
    """
    FORMAT LAYER: Wrap computed weights with metadata for JSON serialization.
    
    Separates domain data from presentation structure.
    """
    return {
        "active": "default",
        "profiles": profiles,
        "_comment": "Generated from Agda TechnicalDebt.Priorities. Maps proof→postulate, documentation→todo, safety→fixme, critical→deviation.",
    }


def generate_bridge_config() -> dict:
    """
    Convenience function combining logic and format layers.
    (For backward compatibility)
    """
    profiles = build_weight_profiles()
    return format_weights_json(profiles)


# ============================================================================
# Validation and Documentation
# ============================================================================

def validate_against_current_weights(current_file: Path) -> dict:
    """Compare generated weights with current weights.json for consistency."""
    if not current_file.exists():
        return {"status": "current weights not found"}
    
    with open(current_file) as f:
        current = json.load(f)
    
    generated = generate_bridge_config()
    
    comparison = {}
    for strategy_name in AGDA_STRATEGIES.keys():
        if strategy_name not in current.get("profiles", {}):
            comparison[strategy_name] = "NEW (not in current weights)"
            continue
        
        current_weights = current["profiles"][strategy_name]
        generated_weights = generated["profiles"][strategy_name]
        
        # Calculate relative differences
        diffs = {}
        for key in ["postulate", "todo", "fixme", "deviation"]:
            if key in current_weights and key in generated_weights:
                curr_val = current_weights[key]
                gen_val = generated_weights[key]
                # Percentage difference
                if curr_val > 0:
                    pct_diff = abs(gen_val - curr_val) / curr_val * 100
                    diffs[key] = f"{pct_diff:.1f}% change"
        
        comparison[strategy_name] = diffs if diffs else "identical"
    
    return comparison


# ============================================================================
# Main
# ============================================================================

if __name__ == "__main__":
    import sys
    
    repo_root = Path(__file__).parent.parent
    badges_dir = repo_root / ".github" / "badges"
    weights_file = badges_dir / "weights.json"
    
    # Generate weights
    generated = generate_bridge_config()
    
    # Validate
    comparison = validate_against_current_weights(weights_file)
    
    print("=" * 80)
    print("PRIO-ADOPT-1: Agda Priority Strategy → Python Badge Weights")
    print("=" * 80)
    print()
    print("Generated profiles from Agda TechnicalDebt.Priorities:")
    print(json.dumps(generated, indent=2))
    print()
    print("Validation against current weights.json:")
    print(json.dumps(comparison, indent=2))
    print()
    print("Mapping reference:")
    print("  postulate ← proof        (Proof obligations, unimplemented algorithms)")
    print("  todo ← documentation    (Documentation TODOs, planned features)")
    print("  fixme ← safety          (Safety issues, bugs, performance problems)")
    print("  deviation ← critical    (Critical deviations, specification mismatches)")
    print()
    
    # Save generated config for reference (don't overwrite current yet)
    reference_file = badges_dir / "weights-agda-mapped.json"
    with open(reference_file, "w") as f:
        json.dump(generated, f, indent=2)
    print(f"Saved reference config: {reference_file}")

