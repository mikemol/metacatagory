#!/usr/bin/env python3
"""
Unit tests for PRIO-ADOPT-1 priority mapping logic

Tests the LOGIC LAYER (pure functions) independent of formatting/I/O.
See adopt_priority_strategies.py for implementation.
"""

import unittest
from pathlib import Path
import sys

# Add scripts directory to path so we can import adopt_priority_strategies
scripts_dir = Path(__file__).parent.parent / "scripts"
sys.path.insert(0, str(scripts_dir))

from adopt_priority_strategies import (
    AGDA_STRATEGIES,
    agda_to_python_weights,
    build_weight_profiles,
    format_weights_json,
)

class TestPriorityMappingLogic(unittest.TestCase):
    """Test LOGIC LAYER: pure mapping functions"""

    def test_agda_strategies_defined(self):
        """All 5 Agda priority strategies should be defined"""
        expected_strategies = {
            "default",
            "ffiSafety",
            "proofCompleteness",
            "rapidDevelopment",
            "production",
        }
        self.assertEqual(set(AGDA_STRATEGIES.keys()), expected_strategies)

    def test_each_strategy_has_all_fields(self):
        """Each strategy should have all 10 priority fields"""
        expected_fields = {
            "minimal",
            "low",
            "medium",
            "high",
            "critical",
            "testFixture",
            "documentation",
            "performance",
            "safety",
            "proof",
        }
        for strategy_name, strategy in AGDA_STRATEGIES.items():
            self.assertEqual(
                set(strategy.keys()),
                expected_fields,
                f"Strategy {strategy_name} missing fields",
            )

    def test_all_weights_are_positive(self):
        """All priority weights should be positive integers"""
        for strategy_name, strategy in AGDA_STRATEGIES.items():
            for field_name, weight in strategy.items():
                self.assertIsInstance(
                    weight,
                    int,
                    f"{strategy_name}.{field_name} should be int",
                )
                self.assertGreater(
                    weight,
                    0,
                    f"{strategy_name}.{field_name} should be positive",
                )

    def test_agda_to_python_weights_returns_dict(self):
        """Mapping function should return dict with 4 categories"""
        for strategy_name in AGDA_STRATEGIES.keys():
            weights = agda_to_python_weights(strategy_name)
            self.assertIsInstance(weights, dict)
            self.assertEqual(set(weights.keys()), {"postulate", "todo", "fixme", "deviation"})

    def test_agda_to_python_weights_all_positive(self):
        """All mapped weights should be positive floats"""
        for strategy_name in AGDA_STRATEGIES.keys():
            weights = agda_to_python_weights(strategy_name)
            for category, weight in weights.items():
                self.assertIsInstance(weight, float, f"{category} should be float")
                self.assertGreater(weight, 0, f"{category} should be positive")

    def test_mapping_consistency(self):
        """Mapping should be consistent: same input → same output"""
        default_weights_1 = agda_to_python_weights("default")
        default_weights_2 = agda_to_python_weights("default")
        self.assertEqual(default_weights_1, default_weights_2)

    def test_mapping_sources(self):
        """Each Python category should map from expected Agda field"""
        # postulate ← proof
        default_proof = AGDA_STRATEGIES["default"]["proof"]
        default_weights = agda_to_python_weights("default")
        expected_postulate = default_proof / 100.0
        self.assertAlmostEqual(default_weights["postulate"], expected_postulate)

        # todo ← documentation
        expected_todo = AGDA_STRATEGIES["default"]["documentation"] / 50.0
        self.assertAlmostEqual(default_weights["todo"], expected_todo)

        # fixme ← safety
        expected_fixme = AGDA_STRATEGIES["default"]["safety"] / 100.0
        self.assertAlmostEqual(default_weights["fixme"], expected_fixme)

        # deviation ← critical
        expected_deviation = AGDA_STRATEGIES["default"]["critical"] / 500.0
        self.assertAlmostEqual(default_weights["deviation"], expected_deviation)

    def test_ffi_safety_emphasizes_fixme(self):
        """FFI safety strategy should weight fixme higher than default"""
        default = agda_to_python_weights("default")
        ffi = agda_to_python_weights("ffiSafety")
        self.assertGreater(ffi["fixme"], default["fixme"])

    def test_ffi_safety_deemphasizes_proof(self):
        """FFI safety strategy should weight postulate lower than default"""
        default = agda_to_python_weights("default")
        ffi = agda_to_python_weights("ffiSafety")
        self.assertLess(ffi["postulate"], default["postulate"])

    def test_proof_completeness_emphasizes_postulate(self):
        """Proof completeness strategy should weight postulate higher than default"""
        default = agda_to_python_weights("default")
        proof = agda_to_python_weights("proofCompleteness")
        self.assertGreater(proof["postulate"], default["postulate"])

    def test_rapid_development_lower_weights(self):
        """Rapid development should have lower overall weights"""
        default = agda_to_python_weights("default")
        rapid = agda_to_python_weights("rapidDevelopment")
        for category in ["postulate", "todo", "fixme", "deviation"]:
            self.assertLess(rapid[category], default[category])

    def test_production_higher_weights(self):
        """Production should have higher overall weights"""
        default = agda_to_python_weights("default")
        prod = agda_to_python_weights("production")
        for category in ["postulate", "todo", "fixme", "deviation"]:
            self.assertGreater(prod[category], default[category])

    def test_build_weight_profiles_completeness(self):
        """build_weight_profiles should return all strategies"""
        profiles = build_weight_profiles()
        self.assertEqual(set(profiles.keys()), set(AGDA_STRATEGIES.keys()))

    def test_build_weight_profiles_consistency(self):
        """build_weight_profiles should match individual strategy calls"""
        profiles = build_weight_profiles()
        for strategy_name in AGDA_STRATEGIES.keys():
            expected = agda_to_python_weights(strategy_name)
            self.assertEqual(profiles[strategy_name], expected)

    def test_unknown_strategy_raises_error(self):
        """Requesting unknown strategy should raise ValueError"""
        with self.assertRaises(ValueError):
            agda_to_python_weights("nonexistent_strategy")

class TestPriorityMappingFormat(unittest.TestCase):
    """Test FORMAT LAYER: serialization functions"""

    def test_format_weights_json_structure(self):
        """Formatted JSON should have expected structure"""
        profiles = build_weight_profiles()
        formatted = format_weights_json(profiles)

        self.assertIn("active", formatted)
        self.assertIn("profiles", formatted)
        self.assertIn("_comment", formatted)
        self.assertEqual(formatted["active"], "default")

    def test_format_preserves_profiles(self):
        """Formatting should not modify the profiles data"""
        profiles = build_weight_profiles()
        formatted = format_weights_json(profiles)

        for strategy_name in profiles.keys():
            self.assertEqual(formatted["profiles"][strategy_name], profiles[strategy_name])

    def test_format_comment_present(self):
        """Formatted output should have clarifying comment"""
        formatted = format_weights_json(build_weight_profiles())
        self.assertIn("Agda", formatted["_comment"])
        self.assertIn("TechnicalDebt.Priorities", formatted["_comment"])

    def test_format_can_be_serialized_to_json(self):
        """Formatted output should be JSON-serializable"""
        import json

        formatted = format_weights_json(build_weight_profiles())
        try:
            json_str = json.dumps(formatted, indent=2)
            # Verify round-trip
            parsed = json.loads(json_str)
            self.assertEqual(parsed, formatted)
        except TypeError as e:
            self.fail(f"Format should be JSON-serializable: {e}")

class TestLogicFormatSeparation(unittest.TestCase):
    """Verify separation of concerns between logic and format layers"""

    def test_logic_functions_have_no_io(self):
        """Logic functions should not perform I/O"""
        # agda_to_python_weights should be callable without filesystem access
        try:
            weights = agda_to_python_weights("default")
            self.assertIsInstance(weights, dict)
        except (FileNotFoundError, OSError):
            self.fail("Logic function should not depend on I/O")

    def test_logic_is_deterministic(self):
        """Logic functions should be deterministic (pure)"""
        results = [agda_to_python_weights("default") for _ in range(10)]
        for result in results[1:]:
            self.assertEqual(result, results[0])

    def test_logic_can_be_tested_independently(self):
        """Logic layer can be tested without format layer"""
        # We should be able to test mapping logic without calling format functions
        weights = agda_to_python_weights("ffiSafety")
        # Pure assertions on logic, independent of format
        self.assertGreater(weights["fixme"], 1.0)
        self.assertLess(weights["postulate"], 1.0)

if __name__ == "__main__":
    unittest.main()
